#![feature(
    const_fmt_arguments_new,
    const_fn_floating_point_arithmetic, is_some_and, btree_drain_filter
)]
#![allow(clippy::too_many_arguments)]
#![cfg_attr(test, feature(assert_matches))]
use anyhow::Context;
use derive_more::FromStr;
use image::{io::Reader as ImageReader, DynamicImage};
use num_traits::FromPrimitive;
use std::{
    collections::{HashMap, HashSet, BTreeMap},
    fmt::Display ,
    fs,
    ops::Deref,
    path::{Path, PathBuf},
    result,
    str::FromStr,
};

use chrono::{Datelike, NaiveDate, Weekday};
use clap::{Parser, ArgEnum};
use colors::Color;
use font_kit::{family_name::FamilyName, properties::Properties, source::SystemSource};
use lazy_static::lazy_static;
use rusttype::{point, Scale};
use serde::{
    de::{self, Visitor, Error as _},
    Deserialize, Deserializer,
};
use serde_with::{serde_as, DisplayFromStr, DeserializeFromStr};
use svg::{
    node::{
        self,
        element::{Circle, Group, Image, Rectangle, TSpan, Text},
    },
    save, Document, Node,
};

mod colors;

type Result<T=(), E=anyhow::Error> = result::Result<T,E>;


#[serde_as]
#[derive(Deserialize)]
struct DateRange{
    #[serde_as(as = "DisplayFromStr")]
    start: NaiveDate,
    #[serde_as(as = "DisplayFromStr")]
    end: NaiveDate
}

impl DateRange {
    fn contains(&self, date: NaiveDate) -> bool {
        self.start <= date && self.end >= date
    }
}

type HolidayConfig = BTreeMap<String, Holidays>;

#[derive(Deserialize)]
struct Holidays {
    name: String,
    color: Color,
    #[serde(default)]
    holidays: Vec<DateRange>,
}

impl HolidayConfigImpl for HolidayConfig {
    fn get_labels(&self) -> Vec<(&Color, &str)>{
        self.values().map(|Holidays { name, color,..}| (color, name.as_str())).collect()
    }

    fn get_on_date(&self, date: NaiveDate) -> Vec<&Color> {
        self.values()
            .filter_map(
                |Holidays {
                     color, holidays, ..
                 }| {
                    if holidays.iter().any(|dr| dr.contains(date)) {
                        Some(color)
                    } else {
                        None
                    }
                },
            )
            .collect()
    }
}
trait HolidayConfigImpl {
    fn get_labels(&self) -> Vec<(&Color, &str)>;
    fn get_on_date(&self, date: NaiveDate) -> Vec<&Color>;
}

#[derive(Deserialize, Debug)]
struct SpecialDays {
    general: Feiertage,
    // birthdays: HashMap<String, String>,
}
#[derive(Debug)]
struct Feiertage(HashMap<NaiveDate, Vec<SpecialDay>>);

impl<'de> Deserialize<'de> for Feiertage {
    fn deserialize<D>(deserializer: D) -> result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Vis;

        impl<'de> Visitor<'de> for Vis {
            type Value = Feiertage;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a map with string keys.")
            }

            fn visit_map<A>(self, mut map: A) -> result::Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut out = HashMap::<_, Vec<_>>::new();
                while let Some((key, value)) = map.next_entry::<String, String>()? {
                    let date_length = key.find(|c: char| !(c.is_ascii_digit() || '-' == c)).unwrap_or(key.len());
                    let date = &key[..date_length];
                    let (free, hidden) = key[date_length..].split_once('-').unwrap_or((&key[date_length..], ""));
                    out.entry(
                        NaiveDate::from_str(date).map_err(|e| A::Error::custom(format!("{date}: {e:?}")))?,
).or_default().push(
                        SpecialDay {
                            name: value,
                            free: free.chars().collect(),
                            hidden: hidden.chars().collect(),
                        },
                    );
                }

                Ok(Feiertage(out))
            }
        }

        deserializer.deserialize_map(Vis)
    }
}

impl Feiertage {
    fn get_special_days(&self, day: NaiveDate) -> &[SpecialDay] {
        if let Some(e) = self.0.get(&day) {
            dbg!(e)
        } else {
            &[]
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct N<T = u32>(T);

impl<'de, T> Deserialize<'de> for N<T>
where
    T: FromStr,
    <T as FromStr>::Err: Display,
{
    fn deserialize<D>(deserializer: D) -> std::prelude::v1::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let f: T = s.parse().map_err(de::Error::custom)?;
        Ok(N(f))
    }
}

impl<T> Deref for N<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> From<T> for N<T> {
    fn from(t: T) -> Self {
        N(t)
    }
}

#[derive(Default, Debug)]
struct SpecialDay {
    name: String,
    free: HashSet<char>,
    hidden: HashSet<char>
}

impl SpecialDay {
    fn free(&self, free_markers: &HashSet<char>) -> bool {
        self.free.iter().any(|c|free_markers.contains(c))
    }
    fn hidden(&self, free_markers: &HashSet<char>) -> bool {
        self.hidden.iter().any(|c|free_markers.contains(c))
    }
}

impl Display for SpecialDay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

const PAGE_HEIGHT: f32 = 297.;
const PAGE_WIDTH: f32 = 210.;
const MARGIN: f32 = 5.;
const MARKER_STYLE: &str = "fill:none;stroke:none;stroke-width:0.3;stroke-dasharray:0.5, 1;";
const TITLE_MARGIN_TOP: f32 = 20.;
const TITLE_FONT_SIZE: f32 = 20.;
const FONT_NAME: &str = "Noto Sans";
const FONT_SIZE: f32 = 5.;
lazy_static! {
    static ref FONT_STYLE: String =
        format!("font-weight:500;font-size:{FONT_SIZE};font-family:'{FONT_NAME}';");
}
const IMAGE_HEIGHT: f32 = 95.;
const IMAGE_WIDTH: f32 = 200.;

const HOLLIDAYS_LABEL: &str = "Ferien:";
#[derive(clap::Parser)]
struct Opts {
    #[clap(short, long, arg_enum, case_insensitive = true)]
    month: Option<Month>,
    #[clap(short, long)]
    year: u16,
    #[clap(short, long)]
    special_days: PathBuf,
    #[clap(short, long)]
    holidays: PathBuf,
    #[clap(short, long)]
    localization: PathBuf,
    #[clap(short, long)]
    free_markers: String,
    #[clap(short, long)]
    holiday_markers: Vec<String>,
    #[clap(short, long)]
    outdir: Option<PathBuf>,
    #[clap(short, long)]
    images: PathBuf,
}

#[derive(ArgEnum, Clone, Copy, Debug, PartialEq, Eq, Hash, DeserializeFromStr, FromStr)]
#[repr(u8)]
enum Month {
    #[clap(alias("jan"))]
    January = 1,
    #[clap(alias("feb"))]
    February,
    #[clap(alias("mar"))]
    March,
    #[clap(alias("apr"))]
    April,
    #[clap(alias("may"))]
    May,
    #[clap(alias("jun"))]
    June,
    #[clap(alias("jul"))]
    July,
    #[clap(alias("aug"))]
    August,
    #[clap(alias("sep"))]
    September,
    #[clap(alias("oct"))]
    October,
    #[clap(alias("nov"))]
    November,
    #[clap(alias("dec"))]
    December,
}

#[serde_as]
#[derive(Clone, Debug, Deserialize, Default)]
#[serde(default)]
struct Localization {
    month: HashMap<Month, String>,
    #[serde_as(as = "HashMap<DisplayFromStr, _>")]
    week_day: HashMap<Weekday, String>
}

fn main() -> Result {
    let Opts { month, year, special_days, holidays, localization, free_markers, holiday_markers, outdir, images } = Opts::parse();

    let SpecialDays {
        general: special_days,
        ..
    } = toml::from_str(&fs::read_to_string(special_days).unwrap()).unwrap();
    let mut holidays: HolidayConfig =
        toml::from_str(&fs::read_to_string(holidays).unwrap()).unwrap();
    let localization = toml::from_str(&fs::read_to_string(localization).unwrap()).unwrap();

    if let Some(outdir) = &outdir {
        fs::create_dir_all(outdir)?;
    }

    let free_markers = free_markers.chars().collect();

    let holiday_marksers: HashSet<_> = holiday_markers.into_iter().collect();

    holidays.drain_filter(|key,_| !holiday_marksers.contains(key)).count();

    if let Some(month) = month {
        let document = document(
            &holidays,
            month,
            year,
            &special_days,
            &free_markers,
            &images,
            &localization,
        )?;
        let txt = format!(
            r#"<?xml version="1.0" standalone="no" ?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN" "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
{document}"#
        );
        if let Some(outdir) = outdir {
            save(outdir.join(format!("{:02}.svg", month as u8)), &document).unwrap();
        } else {
            println!("{txt}");
        }
    } else {
        let outdir = outdir.context("specify an `--outdir` for multiple months")?;
        for month in 1..=12u8 {
            println!("Month: {month}:");
            let document = document(
                &holidays,
                unsafe { ::std::mem::transmute(month) },
                year,
                &special_days,
            &free_markers,
                &images,
                &localization,
            )?;
            save(outdir.join(format!("{month:02}.svg")), &document).unwrap();
        }
    }
        Ok(())
}

fn document(
    holidays: &HolidayConfig,
    month: Month,
    year: u16,
    special_days: &Feiertage,
    free_markers: &HashSet<char>,
    img_dir: &Path,
    localization: &Localization,
) -> Result<Document> {
    let img = img_dir.join(format!("{:02}.png", month as u8));
    let img = if img.exists() {
        img
    } else if img.with_extension("jpg").exists() {
        img.with_extension("jpg")
    } else {
        img.with_extension("jpeg")
    };
    let img = ImageReader::open(&img)
        .with_context(||format!("Opening image: {}", img.with_extension("{png,jpg,jpeg}").display()))?
        .decode()
        .unwrap();

    // let document =
    Ok(Document::new()
        .set("viewBox", (0, 0, PAGE_WIDTH, PAGE_HEIGHT))
        .set("width", format!("{PAGE_WIDTH}mm"))
        .set("height", format!("{PAGE_HEIGHT}mm"))
        .add(title(
            &localization
                .month
                .get(&month)
                .cloned()
                .unwrap_or_else(|| format!("{month:?}")),
        ))
        .add(markers())
        .add(image(&img))
        .add(footer(holidays))
        .add(body(year, month, special_days, holidays, free_markers, localization)))

    //     format!(
    //         r#"<?xml version="1.0" standalone="no" ?>
    // <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN" "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
    // {document}"#
    //     )
}

fn title(month: &str) -> Text {
    Text::new()
        .add(node::Text::new(month))
        .set("style", format!("font-weight:900;font-size:{TITLE_FONT_SIZE};font-family:'{FONT_NAME}';text-anchor:middle;"))
        .set("dominant-baseline", "hanging")
        .set("x", PAGE_WIDTH/2.).set("y", TITLE_MARGIN_TOP)
}

fn markers() -> Group {
    let circle = Circle::new()
        .set("cy", 12f32)
        .set("style", MARKER_STYLE)
        .set("r", 3f32);

    let rectangle = Rectangle::new().set("style", MARKER_STYLE);

    Group::new()
        .add(
            rectangle
                .clone()
                .set("x", MARGIN)
                .set("y", MARGIN)
                .set("width", PAGE_WIDTH - 2. * MARGIN)
                .set("height", PAGE_HEIGHT - 2. * MARGIN),
        )
        .add(circle.clone().set("cx", PAGE_WIDTH / 2. - 40.))
        .add(circle.set("cx", PAGE_WIDTH / 2. + 40.))
        .add(
            rectangle
                .set("x", (PAGE_WIDTH - IMAGE_WIDTH) / 2.)
                .set("y", TITLE_MARGIN_TOP + TITLE_FONT_SIZE)
                .set("width", IMAGE_WIDTH)
                .set("height", IMAGE_HEIGHT),
        )
}

fn image(img: &DynamicImage) -> Image {
    let mut buf = vec![];
    img.write_to(&mut buf, image::ImageOutputFormat::Png)
        .unwrap();
    let res_base64 = base64::encode(&buf);

    Image::new()
        .set("x", (PAGE_WIDTH - IMAGE_WIDTH) / 2.)
        .set("y", TITLE_MARGIN_TOP + TITLE_FONT_SIZE)
        .set("width", IMAGE_WIDTH)
        .set("height", IMAGE_HEIGHT)
        .set("preserveAspectRatio", "xMidYMid")
        .set(
            "xlink:href",
            format!("data:image/png;base64,{}", &res_base64),
        )
}

fn footer(holidays: &HolidayConfig) -> Text {
    let mut footer = Text::new()
        .add(TSpan::new().add(node::Text::new(HOLLIDAYS_LABEL)))
        .set("style", &**FONT_STYLE);

    for e in holidays.get_labels() {
        let color = e.0.to_string_na();
        footer.append(
            TSpan::new()
                .set("style", format!("{}fill:{color}", *FONT_STYLE))
                .set("xml:space", "preserve")
                .add(node::Text::new("  ⯀ ")),
        );

        footer.append(TSpan::new().add(text(e.1)));
    }

    let y = PAGE_HEIGHT - MARGIN;
    footer.set("transform", format!("translate({MARGIN},{y})"))
}

fn text<S>(s: S) -> node::Text
where
    S: ToString,
{
    let s = s.to_string().replace('&', "&amp;");
    node::Text::new(s)
}

lazy_static! {
    static ref FONT: rusttype::Font<'static> = {
        let f = SystemSource::new()
            .select_best_match(
                &[FamilyName::Title(FONT_NAME.into())],
                &Properties::default(),
            )
            .unwrap()
            .load()
            .unwrap();
        let f = (*f.copy_font_data().unwrap()).clone();
        rusttype::Font::try_from_vec(f).unwrap()
    };
}

fn width_of(text: &str, size: f32) -> f32 {
    let size = size * 1.4;
    FONT.layout(text, Scale { x: size, y: size }, point(0.0, 0.0))
        .last()
        .map(|g| g.position().x + g.unpositioned().h_metrics().advance_width)
        .unwrap_or(0.0)
}

const BODY_MARGIN_TOP: f32 = TITLE_MARGIN_TOP + TITLE_FONT_SIZE + IMAGE_HEIGHT + MARGIN * 2.;
const BODY_HEIGHT: f32 = PAGE_HEIGHT - BODY_MARGIN_TOP - FONT_SIZE - MARGIN;
const BODY_WIDTH: f32 = PAGE_WIDTH - MARGIN * 2.;
const WEEK_NUM_FONT_SIZE: f32 = 8.;

const WEEK_DAY_FONT_SIZE: f32 = 11.;
const DAY_FONT_SIZE: f32 = 6.;
const INFO_FONT_SIZE: f32 = 4.;
const LINE_WIDTH: f32 = 0.3;

fn body(
    year: u16,
    month: Month,
    special_days: &Feiertage,
    holidays: &HolidayConfig,
    free_markers: &HashSet<char>,
    localization: &Localization
) -> Group {
    let week_num_width = width_of("00", WEEK_NUM_FONT_SIZE) + 2.;
    let column_width = (BODY_WIDTH - week_num_width) / 7.;
    let row_height = (BODY_HEIGHT - WEEK_DAY_FONT_SIZE) / 6.;
    let week_day_style = format!(
        "font-weight:500;font-size:{WEEK_DAY_FONT_SIZE};font-family:'{FONT_NAME}';text-anchor:middle;"
    );
    let week_num_style =
        format!("font-weight:500;font-size:{WEEK_NUM_FONT_SIZE};font-family:'{FONT_NAME}';");
    let mut body = Group::new()
        .add(
            Rectangle::new()
                .set("style", MARKER_STYLE)
                .set("width", BODY_WIDTH)
                .set("height", BODY_HEIGHT),
        )
        .set(
            "transform",
            format!("translate({MARGIN},{BODY_MARGIN_TOP})"),
        );
    let mut week_days = Group::new().set("transform", format!("translate({week_num_width},0)"));

    let mut x = column_width / 2.;

    
    for wd in Weekday::Mon.num_days_from_monday()..=Weekday::Sun.num_days_from_monday() {
        let wd = Weekday::from_u32(wd).unwrap();
        week_days.append(
            Text::new()
                .add(text(localization.week_day.get(&wd).cloned().unwrap_or_else(|| wd.to_string())))
                .set("x", x)
                .set("style", week_day_style.as_ref())
                .set("dominant-baseline", "hanging"),
        );
        x += column_width;
    }
    let start = NaiveDate::from_ymd_opt(year as i32, month as u32, 1).unwrap();
    let start = start.iso_week();
    let start = NaiveDate::from_isoywd_opt(start.year(), start.week(), Weekday::Mon).unwrap();
    let mut y = WEEK_DAY_FONT_SIZE;
    let mut rows: Vec<_> = start
        .iter_weeks()
        .take(6)
        .map(|week| {
            let mut row = Group::new()
                .add(
                    Text::new()
                        .add(text(week.iso_week().week()))
                        .set("style", week_num_style.as_ref())
                        .set("y", row_height / 2.)
                        .set("dominant-baseline", "middle"),
                )
                .set("transform", format!("translate(0,{y})"));
            y += row_height;
            let mut x = week_num_width;
            for day in week.iter_days().take(7) {
                let special_day = special_days.get_special_days(day).iter().find(|sd| !sd.hidden(free_markers));
                let special = day.weekday() == Weekday::Sun || special_day.is_some_and(|sd| sd.free(free_markers));
                let (color, text_color) = match( day.month() == month as u32,  special) {
                    (true, false) => (Color::BLACK, Color::BLACK),
                    (true, true) => (Color::BLACK, Color::DARK_GRAY),
                        (false, false) => 
                    (Color::BLACK.with_opacity(0.5).alpha_over(Color::WHITE),
                    Color::BLACK.with_opacity(0.5).alpha_over(Color::WHITE)),
                    (false, true) => 
                    (Color::BLACK.with_opacity(0.5).alpha_over(Color::WHITE),
                    Color::DARK_GRAY.with_opacity(0.5).alpha_over(Color::WHITE)),
                };
                let (color, text_color) = (color.to_string_na(), text_color.to_string_na());
                let font_weight = if special {
                    "bold"
                } else{
                    "500"
                };
                let holiday_colors = holidays.get_on_date(day);
                let count = holiday_colors.len() as f32;
                let mut color_x = x;
                for color in holiday_colors{
                    let color = if day.month() == month as u32 {
                            *color
                        } else{
                            color.with_opacity(0.5).alpha_over(Color::WHITE)
                        }.to_string_na();
                    row.append(
                        Rectangle::new()
                        .set(
                            "style",
                            format!("fill:{color};",),
                        )
                        .set("width", column_width/count)
                        .set("height", 2)
                        .set("y", row_height-2.)
                        .set("x", color_x),
                        );
                    color_x += column_width/count;
                }
                row.append(
                    Rectangle::new()
                        .set(
                            "style",
                            format!("fill:none;stroke:{color};stroke-width:{LINE_WIDTH};",),
                        )
                        .set("width", column_width)
                        .set("height", row_height)
                        .set("x", x),
                );
                row.append(
                    {

                        let mut t = Text::new()
                        .add(
                            TSpan::new()
                                .set(
                                    "style",
                                    format!("font-weight:{font_weight};font-size:{DAY_FONT_SIZE};fill:{text_color};"),
                                )
                                .set("xml:space", "preserve")
                                .add(text(day.day())),
                        )
                        .set("x", x + 1.)
                        .set("y", DAY_FONT_SIZE)
                        .set(
                            "style",
                            format!(
                                "font-family:'{FONT_NAME}';inline-size:{};line-height:0.3;",
                                column_width - 2.
                            ),
                        ); 
                        if let Some(special_day) = special_day{
                            t.append(
                                TSpan::new()
                                    .set(
                                        "style",
                                        format!("font-size:{INFO_FONT_SIZE};font-weight:300;fill:{color};"),
                                    )
                                    .set("xml:space", "preserve")
                                    .add(text(format!(" {special_day}"))),
                            );
                        }
                        t
                    }
                );
                x += column_width;
            }
            row
        })
        .collect();
    // Swap rows to fix render order: gray over black
    rows.swap(5, 3);
    rows.swap(4, 5);

    for row in rows {
        body.append(row);
    }
    body.add(week_days)
}

#![feature(
    format_args_capture,
    const_fmt_arguments_new,
    const_fn_floating_point_arithmetic
)]
#![cfg_attr(test, feature(assert_matches))]
use std::{collections::HashMap, fmt::Display, fs, ops::Deref, path::PathBuf, str::FromStr};

use chrono::{Datelike, NaiveDate, Weekday};
use clap::*;
use colors::Color;
use font_kit::{family_name::FamilyName, properties::Properties, source::SystemSource};
use lazy_static::lazy_static;
use rusttype::{point, Scale};
use serde::{de, Deserialize, Deserializer};
use serde_with::{serde_as, DisplayFromStr};
use svg::{
    node::{
        self,
        element::{Circle, Group, Rectangle, TSpan, Text},
    },
    Document, Node,
};

mod colors;


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

type HolidayConfig = HashMap<String, Holidays>;

#[derive(Deserialize)]
struct Holidays {
    name: String,
    color: Color,
    holidays: Vec<DateRange>
}

impl HolidayConfigImpl for HolidayConfig {
    fn get_labels(&self) -> Vec<(&Color, &str)>{
        self.values().map(|Holidays { name, color,..}| (color, name.as_str())).collect()
    }

    fn get_on_date(&self, date:NaiveDate) -> Vec<&Color> {
        self.values().filter_map(|Holidays {color, holidays, ..}| 
                                 if holidays.iter().any(|dr|dr.contains(date)){
                                 Some(color)}else{None}).collect()
    }
}
trait HolidayConfigImpl {
    fn get_labels(&self) -> Vec<(&Color, &str)>;
    fn get_on_date(&self, date:NaiveDate) -> Vec<&Color>;
}
    

type SpecialDays = HashMap<N<i32>, HashMap<N, HashMap<N, SpecialDay>>>;

impl GetSpecialDay for SpecialDays{
    fn get_special_day(&self, day: NaiveDate) -> Option<&SpecialDay> {
        self.get(&day.year().into())
            .and_then(|y| y.get(&day.month().into()))
            .and_then(|m| m.get(&day.day().into()))
    }
}

trait GetSpecialDay {
    
    fn get_special_day(&self, day: NaiveDate) -> Option<&SpecialDay>;
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct N<T = u32>(T);

impl<'de, T> Deserialize<'de> for N<T>
where
    T: FromStr, <T as FromStr>::Err: Display,
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

#[derive(Deserialize)]
#[serde(untagged)]
enum SpecialDay {
    Simple(String),
    Full {
        name: String,
        #[serde(default)]
        free: bool,
    },
}

impl SpecialDay {
    fn free(&self) -> bool {
        match self {
            SpecialDay::Full { free, .. } => *free,
            _ => false,
        }
    }
}

impl Display for SpecialDay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpecialDay::Simple(name) | SpecialDay::Full { name, .. } => name.fmt(f),
        }
    }
}

const PAGE_HEIGHT: f32 = 297.;
const PAGE_WIDTH: f32 = 210.;
const MARGIN: f32 = 5.;
const MARKER_STYLE: &str = "fill:none;stroke:black;stroke-width:0.3;stroke-dasharray:0.5, 1;";
const TITLE_MARGIN_TOP: f32 = 20.;
const TITLE_FONT_SIZE: f32 = 20.;
const FONT_NAME: &str = "Noto Sans";
const FONT_SIZE: f32 = 5.;
lazy_static! {
    static ref FONT_STYLE: String =
        format!("font-weight:500;font-size:{FONT_SIZE};font-family:'{FONT_NAME}';");
}
const IMAGE_HEIGHT: f32 = 90.;
const IMAGE_WIDTH: f32 = 130.;

const HOLLIDAYS_LABEL: &str = "Ferien:";
// const HOLLIDAYS: &[(Color, &str)] = &[
//     (Color::rgb(0, 0, 0xE7), "Adrian"),
//     // (Color::rgb(r, g, b)"#feda00", "Silvia"),
//     // (Color::rgb(r, g, b)"#06cc00", "Rufus & Karl"),
//     // (Color::rgb(r, g, b)"#0000ff", "Roland"),
//     // (Color::rgb(r, g, b)"#00e5d4", "Lea"),
// ];
#[derive(clap::Parser)]
struct Opts {
    #[clap(short, long, arg_enum, case_insensitive = true)]
    month: Month,
    #[clap(short, long)]
    year: u16,
    #[clap(short, long)]
    special_days: PathBuf,
    #[clap(short, long)]
    holidays: PathBuf,
}

#[derive(ArgEnum, Clone, Copy, Debug)]
enum Month {
    #[clap(alias("jan"))]
    Januar = 1,
    #[clap(alias("feb"))]
    Februar,
    #[clap(alias("mar"))]
    März,
    #[clap(alias("may"))]
    Mai,
    #[clap(alias("jun"))]
    Juni,
    #[clap(alias("jul"))]
    Juli,
    #[clap(alias("aug"))]
    August,
    #[clap(alias("sep"))]
    September,
    #[clap(alias("oct"))]
    Oktober,
    #[clap(alias("nov"))]
    November,
    #[clap(alias("dec"))]
    Dezember,
}

fn main() {
    let opts: Opts = Opts::parse();

    let special_days: SpecialDays =
        toml::from_str(&fs::read_to_string(opts.special_days).unwrap()).unwrap();
    let holidays: HolidayConfig =
        toml::from_str(&fs::read_to_string(opts.holidays).unwrap()).unwrap();

    let document = Document::new()
        .set("viewBox", (0, 0, PAGE_WIDTH, PAGE_HEIGHT))
        .set("width", format!("{}mm", PAGE_WIDTH))
        .set("height", format!("{}mm", PAGE_HEIGHT))
        .add(title(opts.month))
        .add(markers())
        .add(footer(&holidays))
        .add(body(opts.year, opts.month, &special_days, &holidays));

    // svg::save("test.svg", &document).unwrap(;
    let txt = format!(
        r#"<?xml version="1.0" standalone="no" ?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN" "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
{document}"#
    );
    // eprintln!("{txt}");
    println!("{txt}");
}

fn title(month: Month) -> Text {
    Text::new()
        .add(node::Text::new(format!("{:?}", month)))
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
    let s = s.to_string().replace("&", "&amp;");
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
    let size = size as f32 * 1.4;
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

fn body(year: u16, month: Month, special_days: &SpecialDays, holidays: &HolidayConfig) -> Group {
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

    for wd in ["Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"] {
        week_days.append(
            Text::new()
                .add(text(wd))
                .set("x", x)
                .set("style", week_day_style.as_ref())
                .set("dominant-baseline", "hanging"),
        );
        x += column_width;
    }
    let start = NaiveDate::from_ymd(year as i32, month as u32, 1);
    let start = start.iso_week();
    let start = NaiveDate::from_isoywd(start.year(), start.week(), Weekday::Mon);
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
                let special_day = special_days.get_special_day(day);
                let special = day.weekday() == Weekday::Sun || special_day.map(|sd| sd.free()).unwrap_or(false);
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
                    let color = color.to_string_na();
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
                    dbg!(color_x);
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
                                    .add(text(format!(" {}", special_day))),
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
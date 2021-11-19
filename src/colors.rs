#![allow(dead_code)]
use std::{
    fmt::Display,
    ops::{Add, Mul},
    str::FromStr,
};

use anyhow::bail;
use serde_with::DeserializeFromStr;

#[derive(DeserializeFromStr, Debug, PartialEq, Clone, Copy)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

const fn f2u(f: f32) -> u8 {
    (255.0 * f) as u8
}

const fn u2f(u: u8) -> f32 {
    u as f32 / 255.0
}

/// Constants
impl Color {
    pub const BLACK: Self = Self::rgb(0, 0, 0);
    pub const DARK_GRAY: Self = Self::rgb(100, 100, 100);
    pub const WHITE: Self = Self::rgb(255, 255, 255);
}

impl Color {
    pub const fn rgb(r: u8, g: u8, b: u8) -> Color {
        Color::rgba(r, g, b, 255)
    }

    pub const fn rgbaf(r: f32, g: f32, b: f32, a: f32) -> Color {
        Color {
            r: f2u(r),
            g: f2u(g),
            b: f2u(b),
            a: f2u(a),
        }
    }

    pub const fn rgba(r: u8, g: u8, b: u8, a: u8) -> Color {
        Color { r, g, b, a }
    }

    pub fn to_string_na(self) -> String {
        format!("#{:02X}{:02X}{:02X}", self.r, self.g, self.b)
    }

    pub fn with_opacity(mut self, opacity: f32) -> Self {
        self.a = f2u(opacity);
        self
    }

    pub fn opacity(self) -> f32 {
        u2f(self.a)
    }

    pub fn alpha_over(self, background: Self) -> Self {
        (self * self.opacity()) + (background * (1. - self.opacity()))
    }

    pub fn floats(self) -> (f32, f32, f32, f32) {
        let Color { r, g, b, a } = self;
        (u2f(r), u2f(g), u2f(b), u2f(a))
    }
}

impl Mul<f32> for Color {
    type Output = Color;

    fn mul(self, rhs: f32) -> Self::Output {
        Color {
            r: f2u(u2f(self.r) * rhs),
            g: f2u(u2f(self.g) * rhs),
            b: f2u(u2f(self.b) * rhs),
            // Don't know if this the correct way but it works :thinking:
            a: self.a,
        }
    }
}

impl Add for Color {
    type Output = Color;

    fn add(self, rhs: Self) -> Self::Output {
        Color {
            r: (self.r as u16 + rhs.r as u16).min(255) as u8,
            g: (self.g as u16 + rhs.g as u16).min(255) as u8,
            b: (self.b as u16 + rhs.b as u16).min(255) as u8,
            a: (self.a as u16 + rhs.a as u16).min(255) as u8,
        }
    }
}

impl FromStr for Color {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        dbg!(s.len());
        match s.len() {
            4 => Self::from_str(&(s.to_owned() + "F")),
            5 => Self::from_str(&{
                let mut r = "#".to_owned();
                s[1..5].chars().for_each(|c| {
                    r.push(c);
                    r.push(c);
                });
                r
            }),
            6 => Self::from_str(&("#".to_owned() + s)),
            7 => Self::from_str(&(s.to_owned() + "FF")),
            9 => Ok(Color {
                r: (u8::from_str_radix(&s[1..3], 16)?),
                g: (u8::from_str_radix(&s[3..5], 16)?),
                b: (u8::from_str_radix(&s[5..7], 16)?),
                a: (u8::from_str_radix(&s[7..9], 16)?),
            }),
            _ => bail!("hi"),
        }
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#{:02X}{:02X}{:02X}{:02X}",
            self.r, self.g, self.b, self.a
        )
    }
}

#[cfg(test)]
mod tests {
    use std::{assert_matches::assert_matches, str::FromStr};

    use crate::colors::Color;

    #[test]
    fn encode_color() {
        assert_eq!(Color::rgb(0, 0, 0).to_string(), "#000000FF");
        assert_eq!(Color::rgb(0, 0, 0).to_string_na(), "#000000");
        assert_eq!(Color::rgba(0x12, 0x0F, 0xF0, 0xAA).to_string(), "#120FF0AA");
        assert_eq!(
            Color::rgba(0x12, 0x0F, 0xF0, 0xAA).to_string_na(),
            "#120FF0"
        );
    }

    #[test]
    fn color_parsing() {
        // BLACK
        assert_matches!(
            Color::from_str("#000"),
            Ok(Color {
                r: 0,
                g: 0,
                b: 0,
                a: 0xFF
            })
        );
        assert_matches!(
            Color::from_str("#000F"),
            Ok(Color {
                r: 0,
                g: 0,
                b: 0,
                a: 0xFF
            })
        );
        assert_matches!(
            Color::from_str("#000000"),
            Ok(Color {
                r: 0,
                g: 0,
                b: 0,
                a: 0xFF
            })
        );
        assert_matches!(
            Color::from_str("#000000FF"),
            Ok(Color {
                r: 0,
                g: 0,
                b: 0,
                a: 0xFF
            })
        );
        // Transparent
        assert_matches!(
            Color::from_str("#0000"),
            Ok(Color {
                r: 0,
                g: 0,
                b: 0,
                a: 0
            })
        );
        // RED
        assert_matches!(
            Color::from_str("#F00"),
            Ok(Color {
                r: 0xFF,
                g: 0,
                b: 0,
                a: 0xFF
            })
        );
        assert_matches!(
            Color::from_str("#400"),
            Ok(Color {
                r: 0x44,
                g: 0,
                b: 0,
                a: 0xFF
            })
        );
        assert_matches!(
            Color::from_str("#440000"),
            Ok(Color {
                r: 0x44,
                g: 0,
                b: 0,
                a: 0xFF
            })
        );
        // RAND
        assert_matches!(
            Color::from_str("#16813554"),
            Ok(Color {
                r: 0x16,
                g: 0x81,
                b: 0x35,
                a: 0x54
            })
        );
    }
}

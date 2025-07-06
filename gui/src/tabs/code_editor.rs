use assembler::lex::Token;
use eframe::emath::Align;
use eframe::epaint::text::{LayoutJob, LayoutSection, TextFormat};
use eframe::epaint::{Color32, Stroke};
use egui::scroll_area::ScrollBarVisibility;
use egui::{ScrollArea, Ui};
use std::fmt::Write;

pub fn show(ui: &mut Ui, text: &mut String) {
    ScrollArea::both()
        .scroll_bar_visibility(ScrollBarVisibility::VisibleWhenNeeded)
        .show(ui, |ui| {
            let mut string = String::new();
            let rows = text.matches("\n").count() + 1;
            let width = (rows as f32).log10().floor() as usize + 1;
            for num in 1..=rows {
                string
                    .write_fmt(format_args!("{num: >width$}", width = width))
                    .unwrap();
                if num != rows {
                    string.write_char('\n').unwrap();
                }
            }
            ui.horizontal_top(|ui| {
                ui.spacing_mut().item_spacing.x = 0.0;
                egui::TextEdit::multiline(&mut string)
                    .code_editor()
                    .desired_rows(rows)
                    .interactive(false)
                    .desired_width(width as f32 * 14.0 * 0.5)
                    .layouter(&mut |ui: &egui::Ui, str, _w| {
                        ui.fonts(|f| {
                            f.layout_job(LayoutJob::single_section(str.to_string(), COMMENT))
                        })
                    })
                    .show(ui);

                egui::TextEdit::multiline(text)
                    .desired_width(f32::INFINITY)
                    .code_editor()
                    .desired_rows(rows)
                    .layouter(&mut |ui: &egui::Ui, str, _w| {
                        ui.fonts(|f| f.layout_job(highlight(ui, str)))
                    })
                    .show(ui);
            })
        });
}

const fn simple_format(color: Color32, underline: bool) -> TextFormat {
    TextFormat {
        font_id: egui::FontId::monospace(14.0),
        extra_letter_spacing: 0.0,
        line_height: None,
        color,
        background: Color32::TRANSPARENT,
        italics: false,
        underline: if underline {
            Stroke {
                width: 1.0,
                color: Color32::RED,
            }
        } else {
            Stroke::NONE
        },
        strikethrough: Stroke::NONE,
        valign: Align::Min,
    }
}
const PUNC: TextFormat = simple_format(Color32::LIGHT_GRAY, false);
const STR: TextFormat = simple_format(Color32::from_rgb(0x40, 0xd0, 0x80), false);
const KEYWORD: TextFormat = simple_format(Color32::from_rgb(0xe0, 0x90, 0x40), false);
const IDENT: TextFormat = simple_format(Color32::LIGHT_GRAY, false);
const LABEL: TextFormat = simple_format(Color32::from_rgb(0x20, 0xc0, 0xb0), false);
const NUMBER: TextFormat = simple_format(Color32::from_rgb(0x00, 0xb0, 0xc0), false);
const PREPROCESSOR: TextFormat = simple_format(Color32::from_rgb(0xe0, 0xd0, 0x00), false);
const LEX_ERROR: TextFormat = simple_format(Color32::from_gray(120), true);
const COMMENT: TextFormat = simple_format(Color32::from_gray(120), false);
const REGISTER: TextFormat = simple_format(Color32::from_rgb(0xa0, 0x30, 0x50), false);

fn highlight(_: &Ui, str: &str) -> LayoutJob {
    let mut layout_job = LayoutJob {
        text: str.into(),
        ..Default::default()
    };

    let mut start = 0;
    for tok in assembler::lex::Lexer::new(str).include_comments() {
        let (byte_range, format) = match tok {
            Ok(ok) => {
                let format = match ok.val {
                    Token::LPar => PUNC,
                    Token::RPar => PUNC,
                    Token::LBrace => PUNC,
                    Token::RBrace => PUNC,
                    Token::LBracket => PUNC,
                    Token::RBracket => PUNC,
                    Token::Plus => PUNC,
                    Token::Minus => PUNC,
                    Token::Star => PUNC,
                    Token::Slash => PUNC,
                    Token::Ampersand => PUNC,
                    Token::BitwiseOr => PUNC,
                    Token::BitwiseXor => PUNC,
                    Token::BitwiseNot => PUNC,
                    Token::ShiftLeft => PUNC,
                    Token::ShiftRight => PUNC,
                    Token::LogicalAnd => PUNC,
                    Token::LogicalOr => PUNC,
                    Token::LogicalNot => PUNC,
                    Token::Comma => PUNC,
                    Token::LessThan => PUNC,
                    Token::LessThanEq => PUNC,
                    Token::GreaterThan => PUNC,
                    Token::GreaterThanEq => PUNC,
                    Token::Equals => PUNC,
                    Token::NotEquals => PUNC,
                    Token::NewLine => PUNC,
                    Token::Assignment => PUNC,
                    Token::ModuloEq => PUNC,
                    Token::Percent => PUNC,
                    Token::DivideEq => PUNC,
                    Token::TimesEq => PUNC,
                    Token::MinusEq => PUNC,
                    Token::PlusEq => PUNC,
                    Token::SmallRightArrow => PUNC,
                    Token::BigRightArrow => PUNC,
                    Token::OrEq => PUNC,
                    Token::AndEq => PUNC,
                    Token::XorEq => PUNC,
                    Token::ShiftRightEq => PUNC,
                    Token::ShiftLeftEq => PUNC,
                    Token::PreProcessorTag(_) => PREPROCESSOR,
                    Token::Label(_) => LABEL,
                    Token::Ident(str) => format_ident(str),
                    Token::StringLiteral(_) => STR,
                    Token::NumericLiteral(_) => NUMBER,
                    Token::CharLiteral(_) => STR,
                    Token::FalseLiteral => KEYWORD,
                    Token::TrueLiteral => KEYWORD,
                    Token::SingleLineComment(_) => COMMENT,
                    Token::MultiLineComment(_) => COMMENT,
                };
                (
                    ok.span.offset as usize..(ok.span.offset as usize + ok.span.len as usize),
                    format,
                )
            }
            Err(err) => (
                err.span.offset as usize..(err.span.offset as usize + err.span.len as usize),
                LEX_ERROR,
            ),
        };
        if start != byte_range.start {
            layout_job.sections.push(LayoutSection {
                leading_space: 0.0,
                byte_range: start..byte_range.start,
                format: simple_format(Color32::TRANSPARENT, false),
            });
        }
        start = byte_range.end;

        layout_job.sections.push(LayoutSection {
            leading_space: 0.0,
            byte_range,
            format,
        });
    }

    if start < str.len() {
        layout_job.sections.push(LayoutSection {
            leading_space: 0.0,
            byte_range: start..str.len(),
            format: simple_format(Color32::TRANSPARENT, false),
        });
    }

    layout_job
}

fn format_ident(ident: &str) -> TextFormat {
    match ident {
        "as" => KEYWORD,
        "size" => KEYWORD,
        "align" => KEYWORD,
        "pcrel" => KEYWORD,
        "absolute" => KEYWORD,
        "format" => KEYWORD,

        "lui" => KEYWORD,
        "auipc" => KEYWORD,
        "jal" => KEYWORD,
        "jalr" => KEYWORD,
        "beq" => KEYWORD,
        "bne" => KEYWORD,
        "blt" => KEYWORD,
        "bge" => KEYWORD,
        "bltu" => KEYWORD,
        "bgeu" => KEYWORD,
        "lb" => KEYWORD,
        "lh" => KEYWORD,
        "lw" => KEYWORD,
        "lbu" => KEYWORD,
        "lhu" => KEYWORD,
        "sb" => KEYWORD,
        "sh" => KEYWORD,
        "sw" => KEYWORD,
        "addi" => KEYWORD,
        "li" => KEYWORD,
        "la" => KEYWORD,
        "ecall" => KEYWORD,
        "ebreak" => KEYWORD,
        "add" => KEYWORD,

        ".global" => KEYWORD,
        ".local" => KEYWORD,
        ".weak" => KEYWORD,

        ".info" => KEYWORD,
        ".warning" => KEYWORD,
        ".error" => KEYWORD,

        "i8" => REGISTER,
        "i16" => REGISTER,
        "i32" => REGISTER,
        "i64" => REGISTER,
        "u8" => REGISTER,
        "u16" => REGISTER,
        "u32" => REGISTER,
        "u64" => REGISTER,
        "f32" => REGISTER,
        "f64" => REGISTER,
        "str" => REGISTER,
        "char" => REGISTER,
        "bool" => REGISTER,

        ".space" => KEYWORD,
        ".data" => KEYWORD,
        ".string" => KEYWORD,
        ".stringz" => KEYWORD,
        ".u8" => KEYWORD,
        ".u16" => KEYWORD,
        ".u32" => KEYWORD,
        ".u64" => KEYWORD,
        ".i8" => KEYWORD,
        ".i16" => KEYWORD,
        ".i32" => KEYWORD,
        ".i64" => KEYWORD,
        ".f32" => KEYWORD,
        ".f64" => KEYWORD,
        ".section" => KEYWORD,
        ".org" => KEYWORD,

        "x0" | "x1" | "x2" | "x3" | "x4" | "x5" | "x6" | "x7" | "x8" | "x9" | "x10" | "x11"
        | "x12" | "x13" | "x14" | "x15" | "x16" | "x17" | "x18" | "x19" | "x20" | "x21" | "x22"
        | "x23" | "x24" | "x25" | "x26" | "x27" | "x28" | "x29" | "x30" | "x31" => REGISTER,
        "zero" | "ra" | "sp" | "gp" | "tp" | "t0" | "t1" | "t2" | "s0" | "fp" | "s1" | "a0"
        | "a1" | "a2" | "a3" | "a4" | "a5" | "a6" | "a7" | "s2" | "s3" | "s4" | "s5" | "s6"
        | "s7" | "s8" | "s9" | "s10" | "s11" | "t3" | "t4" | "t5" | "t6" => REGISTER,

        _ => IDENT,
    }
}

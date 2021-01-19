use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
pub use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::{parse, Expr, ExprLit, ExprPath, ExprRange, Ident, Lit, LitInt, RangeLimits, Token};

pub struct RepeatCountInput {
    macro_name: Ident,
    limits: RangeLimits,
    bounds: CountInputBounds,
    args: TokenStream,
}

pub enum CountInputBounds {
    Int(i64, i64),
    Char(char, char),
    Byte(u8, u8),
    Ident(char, char),
}

impl RepeatCountInput {
    pub fn to_tokens(self) -> TokenStream {
        match self.bounds {
            CountInputBounds::Int(from, to) => match self.limits {
                RangeLimits::HalfOpen(..) => self.write_range(from..to),
                RangeLimits::Closed(..) => self.write_range(from..=to),
            },
            CountInputBounds::Char(from, to) => match self.limits {
                RangeLimits::HalfOpen(..) => self.write_range(from..to),
                RangeLimits::Closed(..) => self.write_range(from..=to),
            },
            CountInputBounds::Byte(from, to) => match self.limits {
                RangeLimits::HalfOpen(..) => self.write_range(from..to),
                RangeLimits::Closed(..) => self.write_range(from..=to),
            },
            CountInputBounds::Ident(from, to) => match self.limits {
                RangeLimits::HalfOpen(..) => self
                    .write_range((from..to).map(|i| Ident::new(&i.to_string(), Span::call_site()))),
                RangeLimits::Closed(..) => self.write_range(
                    (from..=to).map(|i| Ident::new(&i.to_string(), Span::call_site())),
                ),
            },
        }
    }

    fn write_range<T, I: Iterator<Item = T>>(&self, range: I) -> TokenStream
    where
        T: ToTokens,
    {
        let macro_name = &self.macro_name;
        let args = &self.args;
        quote! {
            #(
                #macro_name!(#range #args);
            )*
        }
    }
}

impl parse::Parse for RepeatCountInput {
    fn parse(input: parse::ParseStream<'_>) -> parse::Result<Self> {
        let macro_name = input.parse()?;
        let _: Token![,] = input.parse()?;
        let expr: Expr = input.parse()?;
        if let Expr::Range(range) = expr {
            Ok(RepeatCountInput {
                macro_name,
                limits: range.limits.clone(),
                bounds: CountInputBounds::from_range(range)?,
                args: input.parse()?,
            })
        } else {
            Err(parse::Error::new(expr.span(), "Expected range"))
        }
    }
}

impl CountInputBounds {
    fn from_range(range: ExprRange) -> parse::Result<Self> {
        let from = range
            .from
            .as_ref()
            .ok_or_else(|| parse::Error::new(range.span().clone(), "Lower bound is required"))?
            .as_ref();
        let to = range
            .to
            .as_ref()
            .ok_or_else(|| parse::Error::new(range.span().clone(), "Upper bound is required"))?
            .as_ref();

        let count_input = match (from, to) {
            (
                Expr::Lit(ExprLit {
                    lit: Lit::Int(from),
                    ..
                }),
                Expr::Lit(ExprLit {
                    lit: Lit::Int(to), ..
                }),
            ) => {
                let from = int_literal(from)?;
                let to = int_literal(to)?;
                check_range_order(range.span(), from, to)?;
                CountInputBounds::Int(from, to)
            }
            (
                Expr::Lit(ExprLit {
                    lit: Lit::Char(from),
                    ..
                }),
                Expr::Lit(ExprLit {
                    lit: Lit::Char(to), ..
                }),
            ) => {
                let from = from.value();
                let to = to.value();
                check_range_order(range.span(), from, to)?;
                CountInputBounds::Char(from, to)
            }
            (
                Expr::Lit(ExprLit {
                    lit: Lit::Byte(from),
                    ..
                }),
                Expr::Lit(ExprLit {
                    lit: Lit::Byte(to), ..
                }),
            ) => {
                let from = from.value();
                let to = to.value();
                check_range_order(range.span(), from, to)?;
                CountInputBounds::Byte(from, to)
            }
            (Expr::Path(from), Expr::Path(to)) => {
                CountInputBounds::Ident(single_char_ident(from)?, single_char_ident(to)?)
            }
            _ => {
                return Err(parse::Error::new(
                    range.span(),
                    "Bounds must be literals or identifiers and of the same type",
                ))
            }
        };
        Ok(count_input)
    }
}

fn check_range_order<T: PartialOrd>(span: Span, a: T, b: T) -> parse::Result<()> {
    if a <= b {
        Ok(())
    } else {
        Err(parse::Error::new(
            span,
            "Lower bound must be less than or equal to upperr bound",
        ))
    }
}

fn int_literal(lit: &LitInt) -> parse::Result<i64> {
    lit.base10_digits()
        .parse()
        .map_err(|e| parse::Error::new(lit.span().clone(), format!("{}", e)))
}

fn single_char_ident(path: &ExprPath) -> parse::Result<char> {
    if path.path.segments.len() > 1 {
        return Err(parse::Error::new(path.span().clone(), "Not an identifier"));
    }
    if path.qself.is_some() {
        return Err(parse::Error::new(
            path.span().clone(),
            "What are you even trying to do here?",
        ));
    }
    path.path
        .segments
        .iter()
        .next()
        .and_then(|seg| {
            let chars = format!("{}", seg.ident);
            let mut chars = chars.chars();
            chars.next().filter(|_| chars.next().is_none())
        })
        .ok_or_else(|| {
            parse::Error::new(path.span().clone(), "Identifier must be a single character")
        })
}

use macro_tools_core::{parse_macro_input, RepeatCountInput};
use proc_macro::TokenStream;

#[proc_macro]
pub fn repeat_count(tokens: TokenStream) -> TokenStream {
    parse_macro_input!(tokens as RepeatCountInput)
        .to_tokens()
        .into()
}

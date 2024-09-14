pub mod object;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let _obj = object::Object::<u64>::new();
    }
}

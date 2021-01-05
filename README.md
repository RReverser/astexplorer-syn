# astexplorer-syn

This is a wrapper for [`syn`](https://github.com/dtolnay/syn) that provides a JavaScript interface to the Rust parser compiled to WASM with:

```shell
wasm-pack build --target web
```

It's not intended for general usage, but only as a wrapper for ASTExplorer.

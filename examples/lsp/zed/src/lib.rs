use zed_extension_api as zed;

struct MyExtension {}

impl zed::Extension for MyExtension {
    fn new() -> Self
    where
        Self: Sized,
    {
        println!("Hello World!");
        MyExtension {}
    }

    fn language_server_command(&mut self, language_server_id: &zed::LanguageServerId, worktree: &zed::Worktree) -> zed::Result<zed::Command> {
        Ok(zed::Command {
            command: "/usr/local/bin/franca".to_string(),
            args: vec!["-lsp".to_string()],
            env: vec![],
        })
    }
}

zed::register_extension!(MyExtension);

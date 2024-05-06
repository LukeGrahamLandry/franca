use zed_extension_api as zed;

struct MyExtension {}

impl zed::Extension for MyExtension {
    fn new() -> Self
    where
        Self: Sized,
    {
        MyExtension {}
    }

    fn language_server_command(&mut self, language_server_id: &zed::LanguageServerId, worktree: &zed::Worktree) -> zed::Result<zed::Command> {
        Ok(zed::Command {
            command: "/Users/luke/Documents/mods/infered/target/debug/lsp".to_string(),
            args: vec![],
            env: vec![],
        })
    }
}

zed::register_extension!(MyExtension);

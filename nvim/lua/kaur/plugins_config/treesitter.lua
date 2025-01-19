require("nvim-treesitter.configs").setup {
    ensure_installed = {

	    -- Neovim stuff

	    "vim", 
	    "vimdoc", 

	    -- Treesitter stuff 

	    "query", 

	    -- Markdown stuff

	    "markdown", 
	    "markdown_inline", 

	    -- Config stuff

	    "yaml", 
	    "json", 
	    "jsonc",

	    -- Programming stuff

	    "c",
        "rust",

	    -- Stylesheets stuff
	    
	    "css",
        "scss",

	    -- Scripting stuff

	    "bash",
	    "lua", 
	    "python",

        "javascript",
        "typescript",
        "tsx",

	    -- Tmux stuff

	    "tmux",

	    -- Git stuff

	    "git_config", 
	    "git_rebase", 
	    "gitcommit", 
	    "diff", 
	    "gitattributes", 
	    "gitignore", 

    },
    sync_install = false,

    auto_install = true,

    highlight = {
	    enable = true,

	    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
	    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
	    -- Using this option may slow down your editor, and you may see some duplicate highlights.
	    -- Instead of true it can also be a list of languages 
	    additional_vim_regex_highlighting = false,
    },

    indent = {
        enable = true
    },
}

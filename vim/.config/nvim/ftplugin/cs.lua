-- preload parser to make `get_parser(0)` calls work (`cs` filetype != `c_sharp` parser name)
vim.treesitter.get_parser(0, 'c_sharp')

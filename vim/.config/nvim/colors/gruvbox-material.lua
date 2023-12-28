-- Based on sainnhe/gruvbox-material

vim.cmd.highlight 'clear'
if vim.g.syntax_on then
  vim.cmd.syntax 'reset'
end
vim.g.colors_name = 'gruvbox-material'

local black = '#504945'
local red = '#ea6962'
local green = '#a9b665'
local yellow = '#d8a657'
local blue = '#7daea3'
local magenta = '#d3869b'
local cyan = '#89b482'
local white = '#d4be98'
local bright_black = '#504945'
local bright_red = '#ea6962'
local bright_green = '#a9b665'
local bright_yellow = '#d8a657'
local bright_blue = '#7daea3'
local bright_magenta = '#d3869b'
local bright_cyan = '#89b482'
local bright_white = '#d4be98'
vim.g.terminal_color_0 = black
vim.g.terminal_color_1 = red
vim.g.terminal_color_2 = green
vim.g.terminal_color_3 = yellow
vim.g.terminal_color_4 = blue
vim.g.terminal_color_5 = magenta
vim.g.terminal_color_6 = cyan
vim.g.terminal_color_7 = white
vim.g.terminal_color_8 = bright_black
vim.g.terminal_color_9 = bright_red
vim.g.terminal_color_10 = bright_green
vim.g.terminal_color_11 = bright_yellow
vim.g.terminal_color_12 = bright_blue
vim.g.terminal_color_13 = bright_magenta
vim.g.terminal_color_14 = bright_cyan
vim.g.terminal_color_15 = bright_white
local theme = {
  diffIndexLine = { link = 'Purple' },
  diffLine = { link = 'Grey' },
  diffOldFile = { link = 'Yellow' },
  diffChanged = { link = 'Blue' },
  Operator = { fg = '#e78a4e' },
  InclineNormalNC = { fg = '#928374', bg = '#3c3836' },
  TargetFileName = { link = 'Grey' },
  DefinitionCount = { link = 'Grey' },
  ReferencesCount = { link = 'Grey' },
  String = { fg = bright_green },
  Number = { fg = bright_magenta },
  SpecialKey = { fg = bright_black },
  EndOfBuffer = { fg = bright_black, bg = '#1d2021' },
  TermCursor = { reverse = true },
  TermCursorNC = {  },
  NonText = { fg = bright_black },
  Directory = { fg = bright_green },
  ErrorMsg = { underline = true, fg = bright_red, bold = true },
  DiagnosticInformation = { link = 'DiagnosticInfo' },
  Search = { fg = '#1d2021', bg = bright_green },
  CurSearch = {  },
  MoreMsg = { fg = bright_yellow, bold = true },
  ModeMsg = { fg = bright_white, bold = true },
  LineNr = { fg = bright_black },
  LineNrAbove = { link = 'LineNr' },
  LineNrBelow = { link = 'LineNr' },
  CursorLineNr = { fg = '#928374' },
  CursorLineSign = { link = 'SignColumn' },
  DiagnosticWarning = { link = 'DiagnosticWarn' },
  LineDiagTuncateLine = { link = 'Yellow' },
  LspLinesDiagBorder = { link = 'Yellow' },
  DefinitionIcon = { link = 'Blue' },
  DiagnosticVirtualTextHint = { link = 'VirtualTextHint' },
  DiagnosticVirtualTextError = { link = 'VirtualTextError' },
  DiagnosticVirtualTextWarn = { link = 'VirtualTextWarning' },
  WarningMsg = { fg = bright_yellow, bold = true },
  DiagnosticVirtualTextInfo = { link = 'VirtualTextInfo' },
  DiagnosticUnderlineHint = { underline = true, special = '#d3d3d3' },
  DiagnosticUnderlineError = { underline = true, special = '#ff0000' },
  DiagnosticUnderlineWarn = { underline = true, special = '#ffa500' },
  DiagnosticUnderlineInfo = { underline = true, special = '#add8e6' },
  DiagnosticFloatingHint = { link = 'HintFloat' },
  DiagnosticFloatingError = { link = 'ErrorFloat' },
  DiagnosticFloatingWarn = { link = 'WarningFloat' },
  DiagnosticFloatingInfo = { link = 'InfoFloat' },
  DiagnosticSignHint = { link = 'GreenSign' },
  DiagnosticSignError = { link = 'RedSign' },
  DiagnosticSignWarn = { link = 'YellowSign' },
  DiagnosticSignInfo = { link = 'BlueSign' },
  DefinitionPreviewTitle = { fg = bright_blue, bold = true },
  IndentBlanklineSpaceCharBlankline = { link = 'LineNr' },
  IndentBlanklineSpaceChar = { link = 'LineNr' },
  IndentBlanklineChar = { link = 'LineNr' },
  IndentBlanklineContextChar = { link = 'Grey' },
  TroubleCode = { link = 'Grey' },
  TroubleSource = { link = 'Grey' },
  TroubleText = { link = 'Fg' },
  NormalFloat = { fg = '#ddc7a1', bg = '#3c3836' },
  FloatermBorder = { link = 'Grey' },
  LspSignatureActiveParameter = { link = 'Search' },
  Error = { fg = bright_red },
  Warning = {  },
  Blamer = { link = 'Grey' },
  IncSearch = { fg = '#1d2021', bg = bright_red },
  SignatureMarkerText = { link = 'PurpleSign' },
  SignatureMarkText = { link = 'BlueSign' },
  StatusLineTerm = { fg = '#ddc7a1', bg = '#282828' },
  NvimInvalidStringBody = { link = 'NvimStringBody' },
  NvimInvalidEnvironmentSigil = { link = 'NvimInvalidOptionSigil' },
  NvimInvalidFloat = { link = 'NvimInvalidNumber' },
  multiple_cursors_cursor = { link = 'Cursor' },
  PurpleSign = { fg = bright_magenta },
  Orange = { fg = '#e78a4e' },
  Bold = {  },
  Function = { fg = bright_green },
  Keyword = { fg = bright_red },
  Constant = { fg = bright_cyan },
  lspInlayHintsParameter = { link = 'LineNr' },
  lspInlayHintsType = { link = 'LineNr' },
  lspReference = { link = 'CurrentWord' },
  LspHintHighlight = { link = 'HintText' },
  LspInformationHighlight = { link = 'InfoText' },
  LspWarningHighlight = { link = 'WarningText' },
  LspErrorHighlight = { link = 'ErrorText' },
  NvimInvalidStringQuote = { link = 'NvimInvalidString' },
  NvimInvalidString = { link = 'NvimInvalidValue' },
  NvimInvalidEnvironmentName = { link = 'NvimInvalidIdentifier' },
  NvimInvalidOptionScopeDelimiter = { link = 'NvimInvalidIdentifierScopeDelimiter' },
  NvimInvalidOptionScope = { link = 'NvimInvalidIdentifierScope' },
  NvimInvalidOptionName = { link = 'NvimInvalidIdentifier' },
  NvimInvalidOptionSigil = { link = 'NvimInvalidIdentifier' },
  NvimInvalidNumberPrefix = { link = 'NvimInvalidNumber' },
  NvimInvalidNumber = { link = 'NvimInvalidValue' },
  NvimInvalidRegister = { link = 'NvimInvalidValue' },
  NvimInvalidArrow = { link = 'NvimInvalidDelimiter' },
  NvimInvalidComma = { link = 'NvimInvalidDelimiter' },
  NvimInvalidColon = { link = 'NvimInvalidDelimiter' },
  NvimInvalidIdentifierKey = { link = 'NvimInvalidIdentifier' },
  NvimInvalidIdentifierName = { link = 'NvimInvalidIdentifier' },
  MarkdownH3 = {  },
  diffAdded = { link = 'Green' },
  VimGroup = {  },
  Delimeter = {  },
  Terminal = { fg = bright_white, bg = '#1d2021' },
  ErrorText = { undercurl = true, special = bright_red },
  InfoText = { undercurl = true, special = bright_blue },
  GitGutterChange = { link = 'BlueSign' },
  LspCodeLens = { link = 'VirtualTextInfo' },
  LspCodeLensSeparator = { link = 'VirtualTextHint' },
  diffFile = { link = 'Aqua' },
  diffNewFile = { link = 'Orange' },
  diffRemoved = { link = 'Red' },
  ToolbarLine = { fg = '#ddc7a1', bg = '#3c3836' },
  YellowSign = { fg = bright_yellow },
  BlueSign = { fg = bright_blue },
  GreenSign = { fg = bright_green },
  Boolean = { fg = bright_magenta },
  MatchParen = { bg = '#3c3836' },
  Ignore = { fg = '#928374' },
  NvimInternalError = { fg = '#ff0000', bg = '#ff0000' },
  NvimAssignment = { link = 'Operator' },
  NvimPlainAssignment = { link = 'NvimAssignment' },
  NvimAugmentedAssignment = { link = 'NvimAssignment' },
  NvimAssignmentWithAddition = { link = 'NvimAugmentedAssignment' },
  NvimAssignmentWithSubtraction = { link = 'NvimAugmentedAssignment' },
  NvimAssignmentWithConcatenation = { link = 'NvimAugmentedAssignment' },
  NvimOperator = { link = 'Operator' },
  NvimUnaryOperator = { link = 'NvimOperator' },
  NvimUnaryPlus = { link = 'NvimUnaryOperator' },
  NvimUnaryMinus = { link = 'NvimUnaryOperator' },
  NvimNot = { link = 'NvimUnaryOperator' },
  NvimBinaryOperator = { link = 'NvimOperator' },
  NvimComparison = { link = 'NvimBinaryOperator' },
  NvimComparisonModifier = { link = 'NvimComparison' },
  NvimBinaryPlus = { link = 'NvimBinaryOperator' },
  NvimBinaryMinus = { link = 'NvimBinaryOperator' },
  NvimConcat = { link = 'NvimBinaryOperator' },
  NvimConcatOrSubscript = { link = 'NvimConcat' },
  NvimOr = { link = 'NvimBinaryOperator' },
  NvimAnd = { link = 'NvimBinaryOperator' },
  NvimMultiplication = { link = 'NvimBinaryOperator' },
  NvimDivision = { link = 'NvimBinaryOperator' },
  NvimMod = { link = 'NvimBinaryOperator' },
  NvimTernary = { link = 'NvimOperator' },
  NvimTernaryColon = { link = 'NvimTernary' },
  NvimParenthesis = { link = 'Delimiter' },
  NvimLambda = { link = 'NvimParenthesis' },
  NvimNestingParenthesis = { link = 'NvimParenthesis' },
  NvimCallingParenthesis = { link = 'NvimParenthesis' },
  NvimSubscript = { link = 'NvimParenthesis' },
  NvimSubscriptBracket = { link = 'NvimSubscript' },
  NvimSubscriptColon = { link = 'NvimSubscript' },
  NvimCurly = { link = 'NvimSubscript' },
  NvimContainer = { link = 'NvimParenthesis' },
  NvimDict = { link = 'NvimContainer' },
  NvimList = { link = 'NvimContainer' },
  NvimIdentifier = { link = 'Identifier' },
  NvimIdentifierScope = { link = 'NvimIdentifier' },
  NvimIdentifierScopeDelimiter = { link = 'NvimIdentifier' },
  NvimIdentifierName = { link = 'NvimIdentifier' },
  NvimIdentifierKey = { link = 'NvimIdentifier' },
  NvimColon = { link = 'Delimiter' },
  NvimComma = { link = 'Delimiter' },
  NvimArrow = { link = 'Delimiter' },
  NvimRegister = { link = 'SpecialChar' },
  NvimNumber = { link = 'Number' },
  NvimFloat = { link = 'NvimNumber' },
  NvimNumberPrefix = { link = 'Type' },
  NvimOptionSigil = { link = 'Type' },
  NvimOptionName = { link = 'NvimIdentifier' },
  NvimOptionScope = { link = 'NvimIdentifierScope' },
  NvimOptionScopeDelimiter = { link = 'NvimIdentifierScopeDelimiter' },
  NvimEnvironmentSigil = { link = 'NvimOptionSigil' },
  NvimEnvironmentName = { link = 'NvimIdentifier' },
  NvimString = { link = 'String' },
  NvimStringBody = { link = 'NvimString' },
  NvimStringQuote = { link = 'NvimString' },
  NvimStringSpecial = { link = 'SpecialChar' },
  NvimSingleQuote = { link = 'NvimStringQuote' },
  NvimSingleQuotedBody = { link = 'NvimStringBody' },
  NvimSingleQuotedQuote = { link = 'NvimStringSpecial' },
  NvimDoubleQuote = { link = 'NvimStringQuote' },
  NvimDoubleQuotedBody = { link = 'NvimStringBody' },
  NvimDoubleQuotedEscape = { link = 'NvimStringSpecial' },
  NvimFigureBrace = { link = 'NvimInternalError' },
  NvimSingleQuotedUnknownEscape = { link = 'NvimInternalError' },
  NvimSpacing = { link = 'Normal' },
  NvimInvalidSingleQuotedUnknownEscape = { link = 'NvimInternalError' },
  NvimInvalid = { link = 'Error' },
  NvimInvalidAssignment = { link = 'NvimInvalid' },
  NvimInvalidPlainAssignment = { link = 'NvimInvalidAssignment' },
  NvimInvalidAugmentedAssignment = { link = 'NvimInvalidAssignment' },
  NvimInvalidAssignmentWithAddition = { link = 'NvimInvalidAugmentedAssignment' },
  NvimInvalidAssignmentWithSubtraction = { link = 'NvimInvalidAugmentedAssignment' },
  NvimInvalidAssignmentWithConcatenation = { link = 'NvimInvalidAugmentedAssignment' },
  NvimInvalidOperator = { link = 'NvimInvalid' },
  NvimInvalidUnaryOperator = { link = 'NvimInvalidOperator' },
  NvimInvalidUnaryPlus = { link = 'NvimInvalidUnaryOperator' },
  NvimInvalidUnaryMinus = { link = 'NvimInvalidUnaryOperator' },
  NvimInvalidNot = { link = 'NvimInvalidUnaryOperator' },
  NvimInvalidBinaryOperator = { link = 'NvimInvalidOperator' },
  NvimInvalidComparison = { link = 'NvimInvalidBinaryOperator' },
  NvimInvalidComparisonModifier = { link = 'NvimInvalidComparison' },
  NvimInvalidBinaryPlus = { link = 'NvimInvalidBinaryOperator' },
  NvimInvalidBinaryMinus = { link = 'NvimInvalidBinaryOperator' },
  NvimInvalidConcat = { link = 'NvimInvalidBinaryOperator' },
  NvimInvalidConcatOrSubscript = { link = 'NvimInvalidConcat' },
  NvimInvalidOr = { link = 'NvimInvalidBinaryOperator' },
  NvimInvalidAnd = { link = 'NvimInvalidBinaryOperator' },
  NvimInvalidMultiplication = { link = 'NvimInvalidBinaryOperator' },
  NvimInvalidDivision = { link = 'NvimInvalidBinaryOperator' },
  NvimInvalidMod = { link = 'NvimInvalidBinaryOperator' },
  NvimInvalidTernary = { link = 'NvimInvalidOperator' },
  NvimInvalidTernaryColon = { link = 'NvimInvalidTernary' },
  NvimInvalidDelimiter = { link = 'NvimInvalid' },
  NvimInvalidParenthesis = { link = 'NvimInvalidDelimiter' },
  NvimInvalidLambda = { link = 'NvimInvalidParenthesis' },
  NvimInvalidNestingParenthesis = { link = 'NvimInvalidParenthesis' },
  NvimInvalidCallingParenthesis = { link = 'NvimInvalidParenthesis' },
  NvimInvalidSubscript = { link = 'NvimInvalidParenthesis' },
  NvimInvalidSubscriptBracket = { link = 'NvimInvalidSubscript' },
  NvimInvalidSubscriptColon = { link = 'NvimInvalidSubscript' },
  NvimInvalidCurly = { link = 'NvimInvalidSubscript' },
  NvimInvalidContainer = { link = 'NvimInvalidParenthesis' },
  NvimInvalidDict = { link = 'NvimInvalidContainer' },
  NvimInvalidList = { link = 'NvimInvalidContainer' },
  NvimInvalidValue = { link = 'NvimInvalid' },
  NvimInvalidIdentifier = { link = 'NvimInvalidValue' },
  NvimInvalidIdentifierScope = { link = 'NvimInvalidIdentifier' },
  NvimInvalidIdentifierScopeDelimiter = { link = 'NvimInvalidIdentifier' },
  SignColumn = { fg = bright_white },
  CursorLineFold = { link = 'FoldColumn' },
  FoldColumn = { fg = bright_black },
  StatusLine = { fg = '#ddc7a1', bg = '#282828' },
  StatusLineNC = { fg = '#928374', bg = '#282828' },
  WinSeparator = { link = 'VertSplit' },
  VertSplit = { fg = bright_black },
  Title = { fg = '#e78a4e', bold = true },
  Visual = { bg = '#3c3836' },
  VisualNC = {  },
  WildMenu = { fg = '#000000', bg = '#ffff00' },
  Folded = { fg = '#928374', bg = '#282828' },
  DiffAdd = { bg = '#32361a' },
  DiffChange = { bg = '#0d3138' },
  DiffDelete = { bg = '#3c1f1e' },
  DiffText = { fg = '#1d2021', bg = bright_blue },
  Conceal = { fg = bright_black },
  SpellBad = { undercurl = true, special = bright_red },
  NvimInvalidStringSpecial = { link = 'NvimStringSpecial' },
  NvimInvalidSingleQuote = { link = 'NvimInvalidStringQuote' },
  NvimInvalidSingleQuotedBody = { link = 'NvimInvalidStringBody' },
  NvimInvalidSingleQuotedQuote = { link = 'NvimInvalidStringSpecial' },
  NvimInvalidDoubleQuote = { link = 'NvimInvalidStringQuote' },
  NvimInvalidDoubleQuotedBody = { link = 'NvimInvalidStringBody' },
  NvimInvalidDoubleQuotedEscape = { link = 'NvimInvalidStringSpecial' },
  NvimInvalidDoubleQuotedUnknownEscape = { link = 'NvimInvalidValue' },
  NvimInvalidFigureBrace = { link = 'NvimInvalidDelimiter' },
  NvimInvalidSpacing = { link = 'ErrorMsg' },
  NvimDoubleQuotedUnknownEscape = { link = 'NvimInvalidValue' },
  MarkdownBoldDelimiter = {  },
  MarkdownBold = {  },
  MarkdownHeadingRule = {  },
  MarkdownHeading = {  },
  MarkdownHeadingDelimiter = {  },
  MarkdownH6 = {  },
  MarkdownH5 = {  },
  MarkdownH4 = {  },
  Normal = { fg = bright_white, bg = '#1d2021' },
  MarkdownH2 = {  },
  MarkdownH1 = {  },
  MarkdownRule = {  },
  CurrentWord = { bg = '#32302f' },
  HintText = { undercurl = true, special = bright_green },
  WarningText = { undercurl = true, special = bright_yellow },
  MatchParenCur = { bold = true },
  MatchWord = { underline = true },
  MatchWordCur = { underline = true },
  Question = { fg = bright_yellow },
  CurrentWordTwins = { link = 'CurrentWord' },
  CursorWord0 = { link = 'CurrentWord' },
  CursorWord1 = { link = 'CurrentWord' },
  healthError = { link = 'Red' },
  healthSuccess = { link = 'Green' },
  healthWarning = { link = 'Yellow' },
  Yellow = { fg = bright_yellow },
  Fg = { fg = bright_white },
  Blue = { fg = bright_blue },
  Purple = { fg = bright_magenta },
  RedItalic = { fg = bright_red },
  OrangeItalic = { fg = '#e78a4e' },
  YellowItalic = { fg = bright_yellow },
  GreenItalic = { fg = bright_green },
  AquaItalic = { fg = bright_cyan },
  BlueItalic = { fg = bright_blue },
  PurpleItalic = { fg = bright_magenta },
  RedBold = { fg = bright_red },
  OrangeBold = { fg = '#e78a4e' },
  YellowBold = { fg = bright_yellow },
  GreenBold = { fg = bright_green },
  AquaBold = { fg = bright_cyan },
  BlueBold = { fg = bright_blue },
  PurpleBold = { fg = bright_magenta },
  OrangeSign = { fg = '#e78a4e' },
  AquaSign = { fg = bright_cyan },
  ErrorLine = {  },
  WarningLine = {  },
  InfoLine = {  },
  HintLine = {  },
  HopNextKey = { fg = '#e78a4e', bold = true },
  HopNextKey1 = { fg = bright_green, bold = true },
  HopNextKey2 = { link = 'Green' },
  HopPreview = { fg = '#b8bb26', bold = true },
  HopUnmatched = { link = 'Grey' },
  HopCursor = { link = 'Cursor' },
  HighlightedyankRegion = { link = 'Visual' },
  LspErrorVirtual = { link = 'VirtualTextError' },
  LspWarningVirtual = { link = 'VirtualTextWarning' },
  LspInformationVirtual = { link = 'VirtualTextInfo' },
  LspHintVirtual = { link = 'VirtualTextHint' },
  SpellCap = { undercurl = true, special = bright_blue },
  SpellRare = { undercurl = true, special = bright_magenta },
  SpellLocal = { undercurl = true, special = bright_cyan },
  Pmenu = { fg = '#ddc7a1', bg = '#3c3836' },
  PmenuSel = { fg = '#3c3836', bg = '#a89984' },
  PmenuSbar = { bg = '#3c3836' },
  PmenuThumb = { bg = '#7c6f64' },
  TabLine = { fg = '#ddc7a1', bg = bright_black },
  TabLineSel = { fg = '#1d2021', bg = '#a89984' },
  TabLineFill = { fg = '#ddc7a1', bg = '#282828' },
  CursorColumn = { bg = '#282828' },
  CursorLine = { bg = '#282828' },
  ColorColumn = { bg = '#282828' },
  QuickFixLine = { fg = bright_magenta, bold = true },
  Whitespace = { fg = bright_black },
  NormalNC = {  },
  MsgSeparator = { link = 'StatusLine' },
  MsgArea = {  },
  FloatBorder = { fg = '#928374', bg = '#1d2021' },
  WinBar = { bold = true },
  WinBarNC = { link = 'Grey' },
  Grey = { fg = '#928374' },
  Cursor = { reverse = true },
  lCursor = { fg = '#1d2021', bg = bright_white },
  Substitute = { fg = '#1d2021', bg = bright_yellow },
  FloatShadow = { bg = '#000000', blend = 80 },
  FloatShadowThrough = { bg = '#000000', blend = 100 },
  Todo = { italic = true, fg = bright_magenta },
  Character = { fg = bright_green },
  Float = { fg = bright_magenta },
  Identifier = { fg = bright_blue },
  Conditional = { fg = bright_red },
  Statement = { fg = bright_red },
  Repeat = { fg = bright_red },
  Label = { fg = '#e78a4e' },
  Exception = { fg = bright_red },
  Include = { fg = bright_magenta },
  PreProc = { fg = bright_magenta },
  Define = { fg = bright_magenta },
  Macro = { fg = bright_cyan },
  PreCondit = { fg = bright_magenta },
  StorageClass = { fg = '#e78a4e' },
  Structure = { fg = '#e78a4e' },
  Typedef = { fg = bright_red },
  Tag = { fg = '#e78a4e' },
  Special = { fg = bright_yellow },
  SpecialChar = { fg = bright_yellow },
  Delimiter = { fg = bright_white },
  SpecialComment = { italic = true, fg = '#928374' },
  Debug = { fg = '#e78a4e' },
  DiagnosticError = { fg = '#ff0000' },
  DiagnosticWarn = { fg = '#ffa500' },
  DiagnosticInfo = { fg = '#add8e6' },
  DiagnosticHint = { fg = '#d3d3d3' },
  VirtualTextError = { link = 'Grey' },
  VirtualTextWarning = { link = 'Grey' },
  VirtualTextInfo = { link = 'Grey' },
  VirtualTextHint = { link = 'Grey' },
  ErrorFloat = { fg = bright_red, bg = '#3c3836' },
  WarningFloat = { fg = bright_yellow, bg = '#3c3836' },
  InfoFloat = { fg = bright_blue, bg = '#3c3836' },
  HintFloat = { fg = bright_green, bg = '#3c3836' },
  RedSign = { fg = bright_red },
  LspReferenceText = { link = 'CurrentWord' },
  LspReferenceRead = { link = 'CurrentWord' },
  LspReferenceWrite = { link = 'CurrentWord' },
  Underlined = { underline = true },
  Comment = { italic = true, fg = '#928374' },
  Aqua = { fg = bright_cyan },
  Red = { fg = bright_red },
  Green = { fg = bright_green },
  VimOption = {  },
  ToolbarButton = { fg = '#1d2021', bg = '#a89984' },
  StatusLineTermNC = { fg = '#928374', bg = '#282828' },
  GPGHighlightUnknownRecipient = { link = 'ErrorMsg' },
  GPGError = { link = 'ErrorMsg' },
  GPGWarning = { link = 'WarningMsg' },
  MarkdownOrderedListMarker = {  },
  MarkdownListMarker = {  },
  MarkdownCodeBlock = {  },
  MarkdownCodeDelimiter = {  },
  MarkdownCode = {  },
  MarkdownLinkTextDelimiter = {  },
  MarkdownLinkDelimiter = {  },
  MarkdownLinkText = {  },
  MarkdownUrl = {  },
  MarkdownItalicDelimiter = {  },
  MarkdownItalic = {  },
  Italic = {  },
  debugPC = { bg = "#324232" },
  Type = { fg = bright_yellow },
  vCursor = { link = 'Cursor' },
  iCursor = { link = 'Cursor' },
  CursorIM = { link = 'Cursor' },
  VisualNOS = { bg = '#3c3836' },
  debugBreakpoint = { fg = '#1d2021', bg = bright_red },
}
for k, v in pairs(theme) do
  vim.api.nvim_set_hl(0, k, v)
end

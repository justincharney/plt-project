# pat_lexer.py
from pygments.lexer import RegexLexer, bygroups
from pygments.token import Keyword, Name, Operator, String, Number, Punctuation, Comment, Text

class PATLexer(RegexLexer):
    name = 'PAT'
    aliases = ['pat']
    filenames = ['*.pat']

    tokens = {
        'root': [
            (r'//.*?$', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment'),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r'\b(true|false|null)\b', Keyword.Constant),
            (r'\b(func|return|if|else|for|while|break|continue|package|import|const|type|struct)\b', Keyword),
            (r'\b(mut|final|late|private)\b', Keyword.Reserved),
            (r'\b(bool|u8|u16|u32|u64|i8|i16|i32|i64|f32|f64|string|error)\b', Keyword.Type),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name),
            (r'[+\-*/%=!<>|&~^]+', Operator),
            (r'[()\[\]{},.:;]', Punctuation),
            (r'\d+\.\d*|\.\d+|\d+', Number),
            (r'\s+', Text),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
    }

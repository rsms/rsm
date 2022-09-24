// RSM syntax highlighting for highlight.js
const ops = require("../ops.json")
module.exports = function(hljs) {
  const IREGS = Array.apply(null, Array(32)).map((_,n) => "R"+n)
  const FREGS = Array.apply(null, Array(32)).map((_,n) => "F"+n)

  const IDENT_RE = '[A-Za-z$_][0-9A-Za-z$_]*';

  const KEYWORDS = {
    type:     [ "i1",  "i8",  "i16", "i32", "i64", "f32", "f64" ],
    keyword:  [ "fun", "const" ],
    built_in: ops.map(op => op.name),
    literal:  IREGS.concat(FREGS),
  }

  const COMMENTS = [
    hljs.COMMENT('/\\*', '\\*/'),
    { className: 'comment',
      begin: '//',
      end: '$',
      contains: [
        {
          className: 'errormsg',
          begin: /error:/,
          end: /$/,
        },
      ]
    },
  ]

  return {
    name: 'RSM',
    aliases: [],
    keywords: KEYWORDS,
    illegal: '</',
    contains: [
      hljs.SHEBANG({ binary: "rsm" }),

      ...COMMENTS,

      { className: 'string',
        variants: [
          hljs.QUOTE_STRING_MODE, // "str"
          hljs.APOS_STRING_MODE,  // 'c'
        ]
      },

      { className: 'number',
        variants: [
          { begin: hljs.C_NUMBER_RE + '[i]', relevance: 1 },
          hljs.C_NUMBER_MODE
        ]
      },

      {
        className: 'function',
        beginKeywords: 'fun',
        end: '[(]|$',
        returnBegin: true,
        excludeEnd: true,
        keywords: KEYWORDS,
        relevance: 5,
        contains: [
          {
            begin: hljs.UNDERSCORE_IDENT_RE + '\\s*\\(',
            returnBegin: true,
            relevance: 0,
            contains: [ hljs.UNDERSCORE_TITLE_MODE ]
          },
          {
            className: 'type',
            begin: /</,
            end: />/,
            keywords: 'reified',
            relevance: 0
          },
          {
            className: 'params',
            begin: /\(/,
            end: /\)/,
            endsParent: true,
            keywords: KEYWORDS,
            relevance: 0,
            contains: [
              ...COMMENTS
            ]
          },
          ...COMMENTS
        ]
      },

      {
        begin: [
          /const/,
          /\s+/,
          hljs.UNDERSCORE_IDENT_RE
        ],
        className: {
          1: "keyword",
          3: "variable"
        }
      },

      {
        begin: [
          /data/,
          /\s+/,
          hljs.UNDERSCORE_IDENT_RE
        ],
        className: {
          1: "keyword",
          3: "variable"
        }
      },

      {
        className: 'symbol',
        match: '^[ \\t]*[a-z_\\.\\$][a-z0-9_\\.\\$]+:',
      },

    ]
  };
}

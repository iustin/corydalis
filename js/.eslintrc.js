module.exports = {
  'env': {
    'browser': true,
    'es2020': true,
  },
  'extends': [
    'google',
  ],
  'parser': '@typescript-eslint/parser',
  'parserOptions': {
    'ecmaVersion': 11,
  },
  'plugins': [
    '@typescript-eslint',
  ],
  'rules': {
    'indent': ['error', 2,
               {'VariableDeclarator': 'first',
                'FunctionDeclaration': {'parameters': 'first'},
                'FunctionExpression': {'parameters': 'first'},
                'CallExpression': {'arguments': 'first'},
                'ArrayExpression': 'first',
                'ObjectExpression': 'first',
                'ImportDeclaration': 'first',
                'SwitchCase': 1,
               },
    ],
    'spaced-comment': ['error', 'always', {'markers': ['/']}],
    'require-jsdoc': ['off'],
    'new-cap': ['error', {
      'capIsNewExceptions': ['LOG', 'T_START', 'T_STOP'],
    }],
  },
};

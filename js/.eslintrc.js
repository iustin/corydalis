// eslint-disable-next-line no-undef
module.exports = {
  env: {
    browser: true,
    es2020: true,
    jquery: true,
  },
  extends: [
    'eslint:recommended',
    'plugin:@typescript-eslint/recommended',
    'prettier',
  ],
  parser: '@typescript-eslint/parser',
  plugins: ['@typescript-eslint'],
  rules: {
    'require-jsdoc': ['off'],
    'new-cap': [
      'error',
      {
        capIsNewExceptions: ['LOG', 'LOG_GROUP_START', 'LOG_GROUP_END', 'LOG_TABLE', 'T_START', 'T_STOP'],
      },
    ],
  },
  root: true,
};

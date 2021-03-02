module.exports = {
  env: {
    browser: true,
    es2021: true,
  },
  extends: ['google'],
  parser: '@typescript-eslint/parser',
  parserOptions: {
    ecmaVersion: 12,
    sourceType: 'module',
  },
  plugins: ['@typescript-eslint', 'prettier'],
  rules: {
    'object-curly-spacing': ['off'],
    'prettier/prettier': ['error'],
    'no-invalid-this': ['off'],
    // eslint-disable-next-line prettier/prettier
    'indent': ['error', 2, { MemberExpression: 'off' }],
  },
};

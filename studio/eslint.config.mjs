import typescriptEslint from "@typescript-eslint/eslint-plugin";
import tsParser from "@typescript-eslint/parser";
import path from "node:path";
import { fileURLToPath } from "node:url";
import js from "@eslint/js";
import { FlatCompat } from "@eslint/eslintrc";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const compat = new FlatCompat({
  baseDirectory: __dirname,
  recommendedConfig: js.configs.recommended,
  allConfig: js.configs.all,
});

export default [...compat.extends(
  "eslint:recommended",
  "plugin:@typescript-eslint/strict",
  "plugin:@typescript-eslint/recommended",
  "plugin:@typescript-eslint/recommended-requiring-type-checking",
), {
  ignores: [".yalc/"],
},
{
  plugins: {
    "@typescript-eslint": typescriptEslint,
  },

  languageOptions: {
    parser: tsParser,
    ecmaVersion: 5,
    sourceType: "script",

    parserOptions: {
      tsconfigRootDir: __dirname,
      project: ["tsconfig.json"],
      exclude: ["eslint.config.mjs"],
    },
  },

  rules: {
    "brace-style": ["error", "1tbs", {
      allowSingleLine: true,
    }],
    semi: ["error"],
    "array-bracket-spacing": ["error"],
    "arrow-spacing": ["error"],
    "comma-spacing": ["error"],
    "comma-style": ["error"],
    "eol-last": ["error"],
    indent: ["error", 2, {
      SwitchCase: 0,
    }],
    "comma-dangle": ["error", "always-multiline"],
    "no-unused-vars": "off",
    "@typescript-eslint/no-unused-vars": ["error", {
      argsIgnorePattern: "^_",
    }],
    "@typescript-eslint/no-non-null-assertion": "off",
    "@typescript-eslint/no-namespace": "off",
    "@typescript-eslint/ban-types": "off",
    "@typescript-eslint/no-explicit-any": "off",
  },
}];

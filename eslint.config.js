import { defineConfig } from "eslint/config";
import globals from "globals";
import tsParser from "@typescript-eslint/parser";
import typescriptEslint from "@typescript-eslint/eslint-plugin";
import path from "node:path";
import { fileURLToPath } from "node:url";
import js from "@eslint/js";
import { FlatCompat } from "@eslint/eslintrc";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const compat = new FlatCompat({
    baseDirectory: __dirname,
    recommendedConfig: js.configs.recommended,
    allConfig: js.configs.all
});

export default defineConfig([{
    languageOptions: {
        globals: {
            ...globals.browser,
            ...globals.jquery,
        },

        parser: tsParser,
    },

    extends: compat.extends("eslint:recommended", "plugin:@typescript-eslint/recommended", "prettier"),

    plugins: {
        "@typescript-eslint": typescriptEslint,
    },

    rules: {
        "require-jsdoc": ["off"],

        "new-cap": ["error", {
            capIsNewExceptions: ["LOG", "LOG_GROUP", "LOG_GROUP_END", "LOG_TABLE", "T_START", "T_STOP"],
        }],
    },
}]);

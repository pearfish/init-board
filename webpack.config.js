const path = require("path");

const isProd = process.env.NODE_ENV === "production";

module.exports = {
	entry: {
		initBoard: "./lib/js/src/App.js"
	},
	mode: isProd ? "production" : "development",
	output: {
		path: path.join(__dirname, "bundledOutputs"),
		filename: "[name].js"
	}
};

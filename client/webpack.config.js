const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const autoprefixer = require( 'autoprefixer' );
const precss = require( 'precss' );

const PATHS = {
	src: './src/'
}


module.exports = {
	entry: [
		path.join(__dirname, `${PATHS.src}/index.js`)
	],
	output: {
		path: path.join(__dirname, '/dist/'),
		pathinfo: true,
		filename: '[hash].js',
		publicPath: '/'
	},
	resolve: {
		modulesDirectories: [
			'node_modules',
		],
		extensions: [ '', '.js', '.elm']
	},
	module: {
		loaders: [{
			test: /\.(eot|ttf|woff|woff2|svg)$/,
			loader: 'file-loader'
		},
		{
			test: /\.elm$/,
			exclude: [/elm-stuff/, /node_modules/],
			loader:  'elm-hot!elm-webpack?verbose=true&warn=true'
		},
		{
			test: /\.(css)$/,
			loader: 'style-loader!css-loader!postcss-loader'
		}
	]},
	plugins: [
		new HtmlWebpackPlugin({
			template: `${PATHS.src}/index.html`,
			inject: 'body',
			cache: false,
			favicon: `${PATHS.src}/favicon.ico`
		})
	],
	postcss: function () {
		return [precss, autoprefixer({ browsers: 'last 2 versions' })];
	},
	devServer: {
		inline: true,
		progress: true,
		port: 3333,
		contentBase: PATHS.src
	}
}

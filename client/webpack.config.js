const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');

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
			test: /\.(png|jpg|gif)$/,
			loader: 'file'
		},
		{
			test: /\.(eot|svg|ttf|woff(2)?)(\?v=\d+\.\d+\.\d+)?/,
			loader: 'url'
		},
		{
			test: /\.elm$/,
			exclude: [/elm-stuff/, /node_modules/],
			loader:  'elm-hot!elm-webpack?verbose=true&warn=true'
		},
		{
			test: /\.css$/,
		  loader: 'style!css!postcss',
		  exclude: /node_modules/,
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
		return [
			require('postcss-modules-local-by-default'),
			require('postcss-import')({
				addDependencyTo: webpack,
			}),
			require('postcss-cssnext')({
				browsers: ['last 2 versions']
			})
		];
	},
	devServer: {
		inline: true,
		progress: true,
		port: 3333,
		contentBase: PATHS.src
	}
}

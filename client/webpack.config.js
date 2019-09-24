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
	module: {
		rules: [{
			test: /\.(png|jpg|gif)$/,
			use: 'file-loader'
		},
		{
      test: /\.(eot|svg|ttf|woff(2)?)(\?v=\d+\.\d+\.\d+)?/,
      use: 'url-loader'
		},
		{
			test: /\.elm$/,
			exclude: [/elm-stuff/, /node_modules/],
      use: [
        { loader: 'elm-hot-webpack-loader'},
        { loader: 'elm-webpack-loader',
            options: {
                cwd: __dirname,
                debug: false
            }
        }
      ]
		},
		{
			test: /\.css$/,
		  exclude: /node_modules/,
      // loader: 'style!css!postcss',
      use: [ 
        'style-loader', 
        'css-loader', 
        { loader: 'postcss-loader',
          options: {
            ident: 'postcss',
            plugins: (loader) => [
              require('postcss-simple-vars'),
              require('postcss-modules-local-by-default'),
              require('postcss-import')({
                addDependencyTo: webpack,
              }),
              require('postcss-cssnext')({
                browsers: ['last 2 versions']
              })
            ]
          }
        }
      ]
    }
  ]},
	plugins: [
		new HtmlWebpackPlugin({
			template: `${PATHS.src}/index.html`,
			inject: 'body',
			cache: false,
			favicon: `${PATHS.src}/favicon.ico`
    }),
    new webpack.HotModuleReplacementPlugin()
	],
	devServer: {
    inline: true,
    progress: true,
    stats: 'errors-only',
		port: 3333,
		contentBase: PATHS.src
	}
}

const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin')

const SRC_DIR = path.resolve(__dirname, 'src');
const DIST_DIR = path.resolve(__dirname, 'dist');

module.exports = {
  entry: path.join(SRC_DIR, 'index.jsx'),
  output: {
    path: DIST_DIR,
    filename: 'bundle.js',
  },
  watch: true,
  devServer: {
    contentBase: DIST_DIR,
    port: 8080,
    historyApiFallback: {
      index: 'index.html',
    },
  },
  module: {
    loaders: [
      {
        test: /\.jsx?/,
        include: SRC_DIR,
        loader: 'babel-loader',
        query: {
          babelrc: false,
          presets: [ "es2016", "react"] 
        }
      }
    ]
  }
};

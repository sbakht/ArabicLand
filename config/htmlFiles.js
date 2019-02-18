const path = require('path');
const fs = require('fs');

function fromDir(startPath,filter,callback){

    //console.log('Starting from dir '+startPath+'/');

    if (!fs.existsSync(startPath)){
        console.log("no dir ",startPath);
        return;
    }

    var files=fs.readdirSync(startPath);
    for(var i=0;i<files.length;i++){
        var filename=path.join(startPath,files[i]);
        var stat = fs.lstatSync(filename);
        if (stat.isDirectory()){
            fromDir(filename,filter,callback); //recurse
        }
        else if (filter.test(filename)) callback(filename);
    };
};


function buildHtmlPlugin(template, filename) {
    return new HtmlWebpackPlugin({
      inject: true,
      template: template,
      filename: filename,
    });
}

function getDelim(path) {
    if(path.indexOf('/') > -1) {
        return '/';
    }
    return '\\';
}

function getHtmlFiles(cb) {
    const arr = [];
    fromDir('./src/pages',/\.html$/,function(path){
        console.log(path);
        const split = path.split(getDelim(path));
        const filename = split[split.length - 1];
        arr.push(cb(path, filename));
    });
    return arr;
}

module.exports = getHtmlFiles;

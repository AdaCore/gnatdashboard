module.exports = function(ctx) {
    return {
        plugins: [
            require('postcss-smart-import'),
            require('postcss-assets'),
            require('postcss-mixins'),
            require('postcss-simple-vars'),
            require('postcss-cssnext')
        ]
    };
};

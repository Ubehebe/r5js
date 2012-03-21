addEventListener('load', function () {
    var colors = ['red', 'orange', 'yellow', 'green', 'blue', 'indigo', 'violet'];
    var i=0;
    setInterval(function() {
        document.querySelector('.brand').style.backgroundColor = colors[i = (i+1)%colors.length];
    }, 10000);
});
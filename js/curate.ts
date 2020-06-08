/// <reference types="plotly.js"/>

$(document).ready(function() {

    function onDataReceived(series: any) {

        var cameraLayout = {
            xaxis: {
                rangemode: 'nonnegative' as const,
                title: 'File count'
            },
            yaxis: {
                type: 'log' as const,
                exponentformat: 'SI' as const,
                rangemode: 'nonnegative' as const,
                title: 'Size'
            },
            title: 'Camera file/size stats',
        }
        var lensLayout = {
            xaxis: {
                showticklabels: true,
                automargin: true,
            },
            yaxis: {
                type: 'log' as const,
                exponentformat: 'SI' as const,
                rangemode: 'nonnegative' as const,
                title: 'File count',
            },
            showlegend: true,
            title: 'Lens image count',
        }
        var folderLayout = {
            xaxis: {
                type: 'log' as const,
                exponentformat: 'SI' as const,
                rangemode: 'nonnegative' as const,
                title: 'Size'
            },
            yaxis: {
                type: 'log' as const,
                rangemode: 'nonnegative' as const,
                title: 'File count'
            },
            title: 'Folder file/size stats',
        }
        var config = {
            showLink: false,
            sendData: false,
            displaylogo: false,
            modeBarButtonsToRemove: [ 'toImage' as const, 'sendDataToCloud' as const]
        }
        Plotly.newPlot('cameraChart', series.global, cameraLayout, config);
        Plotly.newPlot('lensChart', series.lenses, lensLayout, config);
        Plotly.newPlot('folderChart', series.folders, folderLayout, config);
    }

    const bootdiv = $('#boot');
    const curateurl = bootdiv.data('curate-url');

    $.ajax({
        url: curateurl,
        type: "GET",
        dataType: "json",
        success: onDataReceived
    });
});

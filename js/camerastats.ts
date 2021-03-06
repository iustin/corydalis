/// <reference types="plotly.js"/>

$(document).ready(function() {
  function onDataReceived(series: any) {
    const cameraLayout = {
      yaxis: {
        type: 'log' as const,
        exponentformat: 'SI' as const,
        rangemode: 'nonnegative' as const,
        title: 'Images',
      },
      xaxis: {
        automargin: true,
      },
    };
    const trendsLayout = {
      yaxis: {
        title: 'Images',
      },
      updatemenus: [
        {y: 1,
         yanchor: 'bottom',
         x: 0,
         xanchor: 'left',
         buttons: [{
           method: 'restyle',
           args: ['stackgroup', 'one'],
           label: 'stacked',
         }, {
           method: 'restyle',
           args: ['stackgroup', null],
           label: 'line',
         }],
        },
        {y: 1,
         yanchor: 'bottom',
         x: 0.1,
         xanchor: 'left',
         buttons: [{
           method: 'restyle',
           args: ['groupnorm', ''],
           label: 'absolute',
         }, {
           method: 'restyle',
           args: ['groupnorm', 'percent'],
           label: 'normalized',
         }],
        },
      ],
    };
    const config = {
      showLink: false,
      sendData: false,
      displaylogo: false,
      modeBarButtonsToRemove: ['toImage' as const, 'sendDataToCloud' as const],
    };
    Plotly.newPlot('cameraChart', series.imagecount, cameraLayout, config);
    Plotly.newPlot('trendsChart', series.trends, trendsLayout, config);
  }

  const bootdiv = $('#boot');
  const camerastatsurl = bootdiv.data('camerastats-url');

  $.ajax({
    url: camerastatsurl,
    type: 'GET',
    dataType: 'json',
    success: onDataReceived,
  });
});

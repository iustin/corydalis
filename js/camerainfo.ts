/// <reference types="plotly.js"/>

$(function () {
  function onDataReceived(series: any) {
    const cameraLayout = {
      xaxis: {
        title: 'Focal length',
      },
      yaxis: {
        title: 'Aperture',
        tickprefix: 'f/',
        tickvals: series.ytickvals,
        ticktext: series.yticktext,
        type: 'log' as const,
      },
      hovermode: 'closest' as const,
    };
    const config = {
      showLink: false,
      sendData: false,
      displaylogo: false,
      modeBarButtonsToRemove: ['toImage' as const, 'sendDataToCloud' as const],
    };
    Plotly.newPlot('lensFlApChart', series.lensflap, cameraLayout, config);
  }

  const bootdiv = $('#boot');
  const camerainfourl = bootdiv.data('camerainfo-url');

  $.ajax({
    url: camerainfourl,
    type: 'GET',
    dataType: 'json',
    success: onDataReceived,
  });
});

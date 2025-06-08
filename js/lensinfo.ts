/// <reference types="plotly.js"/>

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function lensInfoReady() {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  function onDataReceived(series: any) {
    const lensLayout = {
      xaxis: {
        title: { text: 'Focal lengths used' },
      },
      yaxis: {
        title: { text: 'Aperture' },
        tickprefix: 'f/',
        tickvals: series.ytickvals,
        ticktext: series.yticktext,
        type: 'log' as const,
      },
      hovermode: 'closest' as const,
    };
    const lensCameraLayout = {
      yaxis: {
        type: 'log' as const,
        exponentformat: 'SI' as const,
        rangemode: 'nonnegative' as const,
        title: { text: 'Images' },
      },
      xaxis: {
        automargin: true,
        title: { text: 'Per camera total images' },
      },
    };
    const restyle: Plotly.UpdateMenuButton['method'] = 'restyle';
    const xLeft: Plotly.UpdateMenu['xanchor'] = 'left';
    const yBottom: Plotly.UpdateMenu['yanchor'] = 'bottom';
    const trendsLayout = {
      xaxis: {
        title: { text: 'Per lens timeline stats' },
      },
      yaxis: {
        title: { text: 'Images' },
      },
      updatemenus: [
        {
          y: 1,
          yanchor: yBottom,
          x: 0,
          xanchor: xLeft,
          buttons: [
            {
              method: restyle,
              args: ['stackgroup', 'one'],
              label: 'stacked',
            },
            {
              method: restyle,
              args: ['stackgroup', null],
              label: 'line',
            },
          ],
        },
        {
          y: 1,
          yanchor: yBottom,
          x: 0.1,
          xanchor: xLeft,
          buttons: [
            {
              method: restyle,
              args: ['groupnorm', ''],
              label: 'absolute',
            },
            {
              method: restyle,
              args: ['groupnorm', 'percent'],
              label: 'normalized',
            },
          ],
        },
      ],
    };
    const config = {
      showLink: false,
      sendData: false,
      displaylogo: false,
      modeBarButtonsToRemove: ['toImage' as const, 'sendDataToCloud' as const],
      responsive: true,
    };
    Plotly.newPlot(
      'lensFlApChart',
      series.lensapfl.lensflap,
      lensLayout,
      config,
    );
    Plotly.newPlot(
      'cameraChart',
      series.trends.imagecount,
      lensCameraLayout,
      config,
    );
    Plotly.newPlot('trendsChart', series.trends.trends, trendsLayout, config);
  }

  const bootdiv = $('#boot');
  const lensinfourl = bootdiv.data('lensinfo-url');

  $.ajax({
    url: lensinfourl,
    type: 'GET',
    dataType: 'json',
    success: onDataReceived,
  });
}

/// <reference types="plotly.js"/>

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function cameraInfoReady() {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  function onDataReceived(series: any) {
    const cameraLayout = {
      xaxis: {
        title: 'Focal lengths used',
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
    const cameraLensLayout = {
      yaxis: {
        type: 'log' as const,
        exponentformat: 'SI' as const,
        rangemode: 'nonnegative' as const,
        title: 'Images',
      },
      xaxis: {
        automargin: true,
        title: 'Per lens total images',
      },
    };
    const restyle: Plotly.UpdateMenuButton['method'] = 'restyle';
    const xLeft: Plotly.UpdateMenu['xanchor'] = 'left';
    const yBottom: Plotly.UpdateMenu['yanchor'] = 'bottom';
    const trendsLayout = {
      xaxis: {
        title: 'Per lens timeline stats',
      },
      yaxis: {
        title: 'Images',
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
      cameraLayout,
      config,
    );
    Plotly.newPlot(
      'cameraChart',
      series.trends.imagecount,
      cameraLensLayout,
      config,
    );
    Plotly.newPlot('trendsChart', series.trends.trends, trendsLayout, config);
  }

  const bootdiv = $('#boot');
  const camerainfourl = bootdiv.data('camerainfo-url');

  $.ajax({
    url: camerainfourl,
    type: 'GET',
    dataType: 'json',
    success: onDataReceived,
  });
}

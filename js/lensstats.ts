/// <reference types="plotly.js"/>

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function lensStatsReady() {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  function onDataReceived(series: any) {
    const lensLayout = {
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
    const restyle: Plotly.UpdateMenuButton['method'] = 'restyle';
    const xLeft: Plotly.UpdateMenu['xanchor'] = 'left';
    const yBottom: Plotly.UpdateMenu['yanchor'] = 'bottom';
    const trendsLayout = {
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
    Plotly.newPlot('lensChart', series.imagecount, lensLayout, config);
    Plotly.newPlot('trendsChart', series.trends, trendsLayout, config);
  }

  const bootdiv = $('#boot');
  const lensstatsurl = bootdiv.data('lensstats-url');

  $.ajax({
    url: lensstatsurl,
    type: 'GET',
    dataType: 'json',
    success: onDataReceived,
  });
}

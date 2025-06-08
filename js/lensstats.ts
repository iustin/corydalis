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
        title: { text: 'Images' },
      },
      xaxis: {
        automargin: true,
      },
    };
    const relayout: Plotly.UpdateMenuButton['method'] = 'relayout';
    const xLeft: Plotly.UpdateMenu['xanchor'] = 'left';
    const yBottom: Plotly.UpdateMenu['yanchor'] = 'bottom';
    const trendsLayout = {
      yaxis: {
        title: { text: 'Images' },
      },
      barmode: 'stack' as const, // default to stacked bars
      updatemenus: [
        {
          y: 1,
          yanchor: yBottom,
          x: 0,
          xanchor: xLeft,
          buttons: [
            {
              method: relayout,
              args: ['barmode', 'stack'],
              label: 'stacked',
            },
            {
              method: relayout,
              args: ['barmode', 'group'],
              label: 'individual',
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
              method: relayout,
              args: ['barnorm', ''],
              label: 'absolute',
            },
            {
              method: relayout,
              args: ['barnorm', 'percent'],
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

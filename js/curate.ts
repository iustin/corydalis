/// <reference types="plotly.js"/>

$(function () {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  function onDataReceived(series: any) {
    const cameraLayout = {
      xaxis: {
        rangemode: 'nonnegative' as const,
        title: 'File count',
      },
      yaxis: {
        type: 'log' as const,
        exponentformat: 'SI' as const,
        rangemode: 'nonnegative' as const,
        title: 'Size',
      },
      title: 'Camera file/size stats',
    };
    const lensLayout = {
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
    };
    const folderLayout = {
      xaxis: {
        type: 'log' as const,
        exponentformat: 'SI' as const,
        rangemode: 'nonnegative' as const,
        title: 'Size',
      },
      yaxis: {
        type: 'log' as const,
        rangemode: 'nonnegative' as const,
        title: 'File count',
      },
      title: 'Folder file/size stats',
    };
    const yearLayout = {
      xaxis: {
        title: 'Year',
        automargin: true,
      },
      yaxis: { title: 'Image count' },
      yaxis2: {
        title: 'Size',
        overlaying: 'y' as const,
        side: 'right' as const,
        exponentformat: 'SI' as const,
      },
      legend: { orientation: 'h' as const },
      title: 'Yearly image count and size',
    };
    const config = {
      showLink: false,
      sendData: false,
      displaylogo: false,
      modeBarButtonsToRemove: ['toImage' as const, 'sendDataToCloud' as const],
    };
    Plotly.newPlot('cameraChart', series.global, cameraLayout, config);
    Plotly.newPlot('lensChart', series.lenses, lensLayout, config);
    Plotly.newPlot('folderChart', series.folders, folderLayout, config);
    Plotly.newPlot('yearChart', series.years, yearLayout, config);
  }

  const bootdiv = $('#boot');
  const curateurl = bootdiv.data('curate-url');

  $.ajax({
    url: curateurl,
    type: 'GET',
    dataType: 'json',
    success: onDataReceived,
  });
});

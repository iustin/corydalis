/// <reference types="plotly.js"/>

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function curateReady() {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  function onDataReceived(series: any) {
    const cameraLayout = {
      xaxis: {
        rangemode: 'nonnegative' as const,
        title: { text: 'File count' },
      },
      yaxis: {
        type: 'log' as const,
        exponentformat: 'SI' as const,
        rangemode: 'nonnegative' as const,
        title: { text: 'Size' },
      },
      title: { text: 'Camera file/size stats' },
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
        title: { text: 'File count' },
      },
      showlegend: true,
      title: { text: 'Lens image count' },
    };
    const folderLayout = {
      xaxis: {
        type: 'log' as const,
        exponentformat: 'SI' as const,
        rangemode: 'nonnegative' as const,
        title: { text: 'Size' },
      },
      yaxis: {
        type: 'log' as const,
        rangemode: 'nonnegative' as const,
        title: { text: 'File count' },
      },
      title: { text: 'Folder file/size stats' },
    };
    const yearLayout = {
      xaxis: {
        title: { text: 'Year' },
        automargin: true,
      },
      yaxis: {
        title: { text: 'Image count' },
        rangemode: 'tozero' as const,
      },
      yaxis2: {
        title: { text: 'Size' },
        overlaying: 'y' as const,
        side: 'right' as const,
        exponentformat: 'SI' as const,
        rangemode: 'tozero' as const,
      },
      legend: { orientation: 'h' as const },
      title: { text: 'Yearly image count and size' },
    };
    const monthLayout = {
      xaxis: {
        title: { text: 'Month' },
        automargin: true,
        type: 'date' as const,
      },
      yaxis: {
        title: { text: 'Image count' },
        rangemode: 'tozero' as const,
      },
      yaxis2: {
        title: { text: 'Size' },
        overlaying: 'y' as const,
        side: 'right' as const,
        exponentformat: 'SI' as const,
        rangemode: 'tozero' as const,
      },
      legend: { orientation: 'h' as const },
      title: { text: 'Monthly image count and size' },
    };
    const config = {
      showLink: false,
      sendData: false,
      displaylogo: false,
      modeBarButtonsToRemove: ['toImage' as const, 'sendDataToCloud' as const],
      responsive: true,
    };
    Plotly.newPlot('cameraChart', series.global, cameraLayout, config);
    Plotly.newPlot('lensChart', series.lenses, lensLayout, config);
    Plotly.newPlot('folderChart', series.folders, folderLayout, config);
    Plotly.newPlot('yearChart', series.years, yearLayout, config);
    Plotly.newPlot('monthChart', series.months, monthLayout, config);
  }

  const bootdiv = $('#boot');
  const curateurl = bootdiv.data('curate-url');

  $.ajax({
    url: curateurl,
    type: 'GET',
    dataType: 'json',
    success: onDataReceived,
  });
}

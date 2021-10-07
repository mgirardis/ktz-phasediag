function [h,x,y,z] = plotISISurf(isiAvg, xLabel, yLabel, cMapFunc, isiThresh, isiBThresh, axisLim)
    [ x, y, z, cData, aData, cLabels ] = getISIMeshPlot(isiAvg, xLabel, yLabel, 'ISIAvg', isiThresh, isiBThresh);
    nColors = length(cLabels);
    h = surf(x, y, z, 'EdgeColor', 'none', 'CData', cData);
    colormap(cMapFunc(nColors));
    xlabel(xLabel)
    ylabel(yLabel)
    colorbar('YTickLabels', cLabels, 'YTick', 1:nColors);
    axis(axisLim);
    %h = surfc(T, xR, isi, 'EdgeColor', 'none', 'FaceAlpha', 0.5);
    %contour(T, xR, isi);
end
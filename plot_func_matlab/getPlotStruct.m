function s = getPlotStruct(x, y, yErr, xLabel, yLabel, xScale, yScale, legendParName, legendParValues, showLegend, plotTitle, XScaleFactor, YScaleFactor)
    if (nargin == 0)
        s.curves = getCurvePlot();
        s.xLabel = [];
        s.yLabel = [];
        s.xScale = [];
        s.yScale = [];
        s.legend = [];
        s.legendValues = [];
        s.title = [];
    else
        if ((nargin < 10) || (isempty(showLegend)))
            showLegend = true;
        end
        if ((nargin < 11) || (isempty(plotTitle)))
            plotTitle = '';
        end
        if ((nargin < 12) || (isempty(XScaleFactor)))
            XScaleFactor = 1;
        end
        if ((nargin < 13) || (isempty(YScaleFactor)))
            YScaleFactor = 1;
        end
        if (iscell(x))
            if (isscalar(showLegend))
                showLegend = repmat(showLegend, 1, numel(x));
            end
            n = numel(x);
            s.curves = repmat(getCurvePlot(), 1, n);
            for i = 1:n
                s.curves(i) = getCurvePlot(x{i}.*XScaleFactor, y{i}.*YScaleFactor, yErr{i}, showLegend(i));
            end
        else
            s.curves = getCurvePlot(x.*XScaleFactor, y.*YScaleFactor, yErr, showLegend);
        end
        if (XScaleFactor ~= 1)
            p = -int32(log10(XScaleFactor));
            xLabel = [xLabel,' $\times10^{',num2str(p,'%d'),'}$'];
        end
        if (YScaleFactor ~= 1)
            p = -int32(log10(YScaleFactor));
            yLabel = [yLabel,' $\times10^{',num2str(p,'%d'),'}$'];
        end
        s.xLabel = xLabel;
        s.yLabel = yLabel;
        s.xScale = xScale;
        s.yScale = yScale;
        s.legend = getLegendCellArr(legendParName, legendParValues);
        s.legendValues = legendParValues;
        s.title = plotTitle;
    end
end

function s = getCurvePlot(x, y, yErr, showLegend)
    if (nargin == 0)
        s.x = [];
        s.y = [];
        s.yErr = getErrorStruct();
        s.showLegend = false;
    else
        s.x = x;
        s.y = y;
        if (~isempty(yErr))
            [m,n] = size(yErr);
            if (m == 1) || (n == 1)
                s.yErr = getErrorStruct(yErr);
            else
                if m == 2
                    yErr = yErr';
                end
                s.yErr = getErrorStruct(yErr(:,1), yErr(:,2));
            end
        else
            s.yErr = [];
        end
        s.showLegend = showLegend;
    end
end

function c = getLegendCellArr(n, v)
    if iscell(v)
        c = v;
    else
        m = numel(v);
        c = cell(1,m);
        for i = 1:m
            c{i} = [ '$', n, '=' , num2str(v(i)), '$' ];
        end
    end
end

function s = getErrorStruct(L,U)
    if (nargin > 1)
        s.L = L(:)';
        s.U = U(:)';
    elseif (nargin == 1)
        s.L = L(:)';
        s.U = s.L;
    else
        s.L = [];
        s.U = [];
    end
end
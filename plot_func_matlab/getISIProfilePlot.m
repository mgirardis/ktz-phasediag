function [x,y,yErr] = getISIProfilePlot(isiData, xLabel, isiCol, pVal, ignoreZeros, xM, yM)
    if (nargin < 5) || isempty(ignoreZeros)
        ignoreZeros = true;
    end
    if (nargin < 6)
        xM = [];
        yM = [];
    end
    [~,x,y] = plotISI(@plot, isiData, xLabel, isiCol, pVal, '');
    if (ignoreZeros)
        ind = (y>0);
        x = x(ind);
        y = y(ind);
    end
    if (~isempty(xM))
        if (numel(xM)>1) && (numel(yM) ~= numel(xM))
            error('getISIProfilePlot:yM', 'yM and xM must have the same number of elements');
        end
        if (numel(xM)>1) && (~iscell(yM))
            error('getISIProfilePlot:yM', 'yM must be a 1,N cell');
        end
        if isempty(find(size(xM)==1,1,'first'))
            error('getISIProfilePlot:xM', 'xM must be a 1,N vector');
        end
        if (numel(xM) == 1)
            if (numel(yM)~=2)
                error('getISIProfilePlot:yM', 'yM must be a 1,2 vector');
            end
            yM = {yM};
        end
        % xM must be a vector containing the values of x that must hold a vertical line
        % the y limits of the vertical lines are in yM, for each corresponding xM
        rx = cell(1,numel(xM)+1);
        ry = cell(1,numel(xM)+1);
        yErr = cell(1,numel(xM)+1);
        for i = 1:numel(xM)
            if any(diff(yM{i})<=0)
                error('getISIProfilePlot:yM', sprintf('yM{%d}(2) must be > yM{%d}(1)', i, i));
            end
            rx{i} = [xM(i),xM(i)];
            ry{i} = [yM{i}(1),yM{i}(2)];
        end
        rx{end} = x;
        ry{end} = y;
        x = rx;
        y = ry;
    else
        yErr = [];
    end
end
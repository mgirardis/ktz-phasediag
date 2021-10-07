function [h,x,y] = plotISI(plotFunc, isiMat, xLabel, isiCol, pVal, dataStyle)
% plots isi data as function of p1 for a given p2Val
% p1Col may be col 1 or 2 (xR or T, respectively)
% and p2Val is the value of the other parameter for which ISI Vs p1 is plotted

    if (strcmp(xLabel, 'xR'))
        p1Col = 1;
        p2Col = 2;
        p2Label = 'T';
    elseif (strcmp(xLabel, 'T'))
        p1Col = 2;
        p2Col = 1;
        p2Label = 'xR';
    else
        error('unrecognized xLabel');
    end

    ind = find(isiMat(:,p2Col) == pVal);
    
    if (nargout > 1)
        h = [];
        x = isiMat(ind, p1Col);
        y = isiMat(ind, isiCol);
    else
        h = figure;
        plotFunc(isiMat(ind,p1Col), isiMat(ind, isiCol), dataStyle);
        xlabel(['$',xLabel,'$'], 'interpreter', 'latex')
        ylabel('ISI', 'interpreter', 'latex')
        text(0.7,0.7, [ '$', p2Label, '=', num2str(pVal,'%.4g'),'$'], 'units', 'normalized', 'interpreter', 'latex');
    end

end
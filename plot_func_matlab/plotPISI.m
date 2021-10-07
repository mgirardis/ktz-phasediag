function h = plotPISI(plotFunc, isiMat, TVal, TCol, xRVal, xRCol, dataStyle)
% plots isi data as function of p1 for a given p2Val
% p1Col may be col 1 or 2 (xR or T, respectively)
% and p2Val is the value of the other parameter for which ISI Vs p1 is plotted

    ind = find((isiMat(:,xRCol) == xRVal) & (isiMat(:,TCol) == TVal));
    
    h = figure;
    plotFunc(isiMat(ind,3), isiMat(ind,4), dataStyle);
    xlabel('$ISI$', 'interpreter', 'latex')
    ylabel('$P(ISI)$', 'interpreter', 'latex')
    text(0.7,0.8, [ '$T =', num2str(TVal,'%.4g'),'$' ], 'units', 'normalized', 'interpreter', 'latex');
    text(0.7,0.7, [ '$x_R =', num2str(xRVal,'%.4g'),'$' ], 'units', 'normalized', 'interpreter', 'latex');
end
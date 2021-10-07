function h = plotISIAvg(plotFunc, isiAvg, xLabel, yLabel, pVal, dataStyle)

    if (strcmp(xLabel, 'xR'))
        pLabel = 'T';
    elseif (strcmp(xLabel, 'T'))
        pLabel = 'xR';
    else
        error('unrecognized xLabel');
    end

    ind = find(isiAvg.(pLabel) == pVal);
    
    h = figure;
    plotFunc(isiAvg.(xLabel)(ind), isiAvg.(yLabel)(ind), dataStyle);
    xlabel(['$',xLabel,'$'], 'interpreter', 'latex')
    ylabel(['$',yLabel,'$'], 'interpreter', 'latex')
    text(0.7,0.7, [ '$', pLabel, '=', num2str(pVal,'%.4g'),'$'], 'units', 'normalized', 'interpreter', 'latex');

end
%% Matlab Plots for ECB SPF
clear 
close all
clc


% Plot options
set(0, 'defaultAxesFontName', 'Times'); 
set(0, 'DefaultAxesFontSize',15)
set(0, 'defaultAxesLineStyleOrder', '-|--|:', 'defaultLineLineWidth', 1.5)
setappdata(0, 'defaultAxesXTickFontSize', 1)
setappdata(0, 'defaultAxesYTickFontSize', 1)
% set(0,'defaultTextInterpreter','latex')

y_range = [-11 5]; 
LegendLocation = 'Southeast';
plot_ratio = [6,4];
FileName = 'SPF_forecast_h';

% Read in plot data
data = readtable('SPF_plot_data.csv');
data(data.target_year < 2001,:) = [];

% Date variable
year = data.target_year;
quarter = data.target_quarter;
month = (quarter - 1) * 3 + 1;
dates = datetime(year, month, 1);

% Dates to be plotted
plot_ind = ~isnan(data.gdp_growth) & data.target_year < 2020;

hfig = figure;
plot(dates(plot_ind),data.gdp_growth(plot_ind),'Linewidth',1.5,'Linestyle','-','color','black')
hold on
plot(dates(plot_ind),data.spf_h0(plot_ind),'Linewidth',1.5,'Linestyle','-','color','#0072BD')
ylim(y_range)
recessionplot

% Legend names specified as strings
legend('real GDP','SPF: $h=0$','interpreter','latex','fontsize',12,'Location',LegendLocation)

% Adjust figure margins
if ~isempty(plot_ratio)
    set(gcf, 'Units', 'Inches', 'Position', [0, 0, plot_ratio(1), plot_ratio(2)], ...
        'PaperUnits', 'Inches', 'PaperSize', plot_ratio)
end
tightfig(hfig);


% Store figures and PNG
fullFileNamepng = FileName + "0"; 
saveas(gcf,fullFileNamepng,'png')




% Loop over forecast horizons
for h = 0:3

    hfig = figure;
    plot(dates(plot_ind),data.gdp_growth(plot_ind),'Linewidth',1.5,'Linestyle','-','color','black')
    hold on
    
    spf_var = data.("spf_h" + h); 
    plot(dates(plot_ind),spf_var(plot_ind),'Linewidth',1.5,'Linestyle','-','color','#0072BD')
    
    ylim(y_range)
    recessionplot
    
    % Legend names specified as strings
    legend('real GDP',"SPF: $h="+h+"$",'interpreter','latex','fontsize',12,'Location',LegendLocation)
    
    % Adjust figure margins
    if ~isempty(plot_ratio)
        set(gcf, 'Units', 'Inches', 'Position', [0, 0, plot_ratio(1), plot_ratio(2)], ...
            'PaperUnits', 'Inches', 'PaperSize', plot_ratio)
    end
    tightfig(hfig);
    
    % Store figures and PNG
    fullFileNamepng = FileName + string(h); 
    saveas(gcf,fullFileNamepng,'png')

end


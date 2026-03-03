%% Matlab Plots for ECB SPF
clear 
close all
clc


% Read in plot data
data = readtable('SPF_plot_data.csv');
data(data.target_year < 2002,:) = [];

% Create date variable
year = data.target_year;
quarter = data.target_quarter;
month = (quarter - 1) * 3 + 1;
dates = datetime(year, month, 1);


% General plot options
set(0, 'defaultAxesFontName', 'Times'); 
set(0, 'DefaultAxesFontSize',15)
set(0, 'defaultAxesLineStyleOrder', '-|--|:', 'defaultLineLineWidth', 1.5)
setappdata(0, 'defaultAxesXTickFontSize', 1)
setappdata(0, 'defaultAxesYTickFontSize', 1)
greytranspar = 0.4;
% set(0,'defaultTextInterpreter','latex')

% CEPR recession periods (year, quarter)
recession_start = datetime([2008 2011 2019],[1 7 10],1);  % year, month, day placeholder
recession_end   = datetime([2009 2013 2020],[4 1 4],1);   % year, month, day placeholder
recession_start = dateshift(recession_start,'start','quarter');
recession_end   = dateshift(recession_end,'start','quarter');


%% Filtered quarterly SPF forecasts 

% Plot options
y_range = [-11 5]; 
plot_ratio = [6,4];
FileName = 'SPF_forecast_h';


% Dates to be plotted
plot_ind = ~isnan(data.gdp_growth) & data.target_year < 2020;

% Loop over forecast horizons
for h = 0:3

    % Create figure
    hfig = figure;
        
    % CEPR recession periods (plot first so lines are on top)
    max_date = max(dates(plot_ind));
    for i = 1:length(recession_start)
        if recession_start(i) < max_date
            patch([recession_start(i) min(recession_end(i),max_date) ...
                   min(recession_end(i),max_date) recession_start(i)], ...
                  [y_range(1) y_range(1) y_range(2) y_range(2)], ...
                  [0.8 0.8 0.8], ...   % gray color
                  'EdgeColor','none', ...
                  'FaceAlpha',greytranspar);    % transparency
            hold on
        end
    end

    % Plot GDP growth
    plot(dates(plot_ind),data.gdp_growth(plot_ind),'Linewidth',1.5,'Linestyle','--','color','black')
    hold on
    
    % Plot SPF h-step-ahead forecast
    spf_var = data.("spf_h" + h); 
    plot(dates(plot_ind),spf_var(plot_ind),'Linewidth',1.5,'Linestyle','-','color','#0072BD')
    
    % Range of y-axis
    ylim(y_range)

    % % Legend names specified as strings
    % LegendLocation = 'Southeast';
    % legend('real GDP',"SPF: $h="+h+"$",'interpreter','latex','fontsize',12,'Location',LegendLocation)
    
    % Adjust figure margins
    if ~isempty(plot_ratio)
        set(gcf, 'Units', 'Inches', 'Position', [0, 0, plot_ratio(1), plot_ratio(2)], ...
            'PaperUnits', 'Inches', 'PaperSize', plot_ratio)
    end
    tightfig(hfig);
    box on 

    % Store figures and PNG
    fullFileNamepng = FileName + string(h); 
    saveas(gcf,fullFileNamepng,'png')

end


% Additional legend as .png
hfig = figure;

% Draw empty plot
p1 = plot(NaN,NaN,'Linewidth',1.5,'Linestyle','--','color','black');
hold on
p2 = plot(NaN,NaN,'Linewidth',1.5,'Linestyle','-','color','#0072BD');

% Remove axis
axis off

% Legend
lgd = legend([p1 p2], {'real GDP','SPF: $h$-quarter-ahead'}, ...
    'Interpreter','latex', ...
    'FontSize',12, ...
    'Location','northoutside', ...
    'Orientation','horizontal');

set(gca,'Visible','off')
set(gcf,'Color','white')

% Get legend size in normalized units
lgd.Units = 'inches';
legendPos = lgd.Position;

% Resize figure to legend size (+ small margin)
set(gcf,'Units','inches')
set(gcf,'Position',[1 1 legendPos(3) legendPos(4)])

% Save tightly
tightfig(hfig);
exportgraphics(gcf,'SPF_legend.png','Resolution',300)



%% Disagreement among filtered quarterly SPF forecasts 

% Plot options
y_range = [0 3]; 
LegendLocation = 'Northwest';
plot_ratio = [11,4];
FileName = 'SPF_disagreement';


% Dates to be plotted
plot_ind = ~isnan(data.gdp_growth);


% Create figure
hfig = figure;
hold on

% CEPR recession periods (plot first so lines are on top)
max_date = max(dates(plot_ind));
for i = 1:length(recession_start)
    if recession_start(i) < max_date
        patch([recession_start(i) min(recession_end(i),max_date) ...
            min(recession_end(i),max_date) recession_start(i)], ...
        [y_range(1) y_range(1) y_range(2) y_range(2)], ...
        [0.8 0.8 0.8], ...   % gray color
        'EdgeColor','none', ...
        'FaceAlpha',greytranspar);    % transparency
    hold on
    end
end


% Plot disagreements for all horizons in one plot
SPF0 = plot(dates(plot_ind),data.IQR_h0(plot_ind),'color','black','Linewidth',2);
SPF1 = plot(dates(plot_ind),data.IQR_h1(plot_ind),'Color',[0.2 0.2 0.2],'LineStyle','--','Linewidth',1.8);
SPF2 = plot(dates(plot_ind),data.IQR_h2(plot_ind),'Color',[0.4 0.4 0.4],'LineStyle',':','Linewidth',1.8);
SPF3 = plot(dates(plot_ind),data.IQR_h3(plot_ind),'Color',[0.6 0.6 0.6],'LineStyle','-.','Linewidth',1.8);
SPF4 = plot(dates(plot_ind),data.IQR_h4(plot_ind),'Color',[0.7 0.7 0.7],'LineStyle','-','Linewidth',1.5);

% Range of y-axis
ylim(y_range)

% Legend names specified as strings
legend([SPF0, SPF1, SPF2, SPF3, SPF4], ...
       {'$h=0$','$h=1$','$h=2$','$h=3$','$h=4$'}, ...
       'Interpreter','latex','FontSize',12,'Location',LegendLocation)

% Adjust figure margins
if ~isempty(plot_ratio)
    set(gcf, 'Units', 'Inches', 'Position', [0, 0, plot_ratio(1), plot_ratio(2)], ...
        'PaperUnits', 'Inches', 'PaperSize', plot_ratio)
end
tightfig(hfig);
box on 

% Store figures and PNG
saveas(gcf,FileName,'png')




%% Filtered quarterly SPF forecasts during COVID-19

% Plot options
y_range = [-43 63]; 
plot_ratio = [6,4];
FileName = 'SPF_forecast_COVID_h';


% Dates to be plotted
plot_ind = ~isnan(data.gdp_growth) & data.target_year > 2018 & data.target_year < 2025;

% Loop over forecast horizons
for h = 0:3

    % Create figure
    hfig = figure;
        
    % CEPR recession periods (plot first so lines are on top)
    max_date = max(dates(plot_ind));
    min_date = min(dates(plot_ind));
    for i = 1:length(recession_start)
        if recession_start(i) < max_date && recession_start(i) > min_date
            patch([recession_start(i) min(recession_end(i),max_date) ...
                   min(recession_end(i),max_date) recession_start(i)], ...
                  [y_range(1) y_range(1) y_range(2) y_range(2)], ...
                  [0.8 0.8 0.8], ...   % gray color
                  'EdgeColor','none', ...
                  'FaceAlpha',greytranspar);    % transparency
            hold on
        end
    end

    % Plot GDP growth
    plot(dates(plot_ind),data.gdp_growth(plot_ind),'Linewidth',1.5,'Linestyle','--','color','black')
    hold on
    
    % Plot SPF h-step-ahead forecast
    spf_var = data.("spf_h" + h); 
    plot(dates(plot_ind),spf_var(plot_ind),'Linewidth',1.5,'Linestyle','-','color','#0072BD')
    
    % Range of y-axis
    ylim(y_range)

    % % Legend names specified as strings
    % LegendLocation = 'Southeast';
    % legend('real GDP',"SPF: $h="+h+"$",'interpreter','latex','fontsize',12,'Location',LegendLocation)
    
    % Adjust figure margins
    if ~isempty(plot_ratio)
        set(gcf, 'Units', 'Inches', 'Position', [0, 0, plot_ratio(1), plot_ratio(2)], ...
            'PaperUnits', 'Inches', 'PaperSize', plot_ratio)
    end
    tightfig(hfig);
    box on 

    % Store figures and PNG
    fullFileNamepng = FileName + string(h); 
    saveas(gcf,fullFileNamepng,'png')

end





%% Filtered quarterly SPF forecasts during COVID-19 incorporating fixed-horizon forecasts

% Plot options
y_range = [-43 63]; 
plot_ratio = [6,4];
FileName = 'SPF_forecast_COVID_FH_h';


% Dates to be plotted
plot_ind = ~isnan(data.gdp_growth) & data.target_year > 2018 & data.target_year < 2025;

% Loop over forecast horizons
for h = 0:3

    % Create figure
    hfig = figure;
        
    % CEPR recession periods (plot first so lines are on top)
    max_date = max(dates(plot_ind));
    min_date = min(dates(plot_ind));
    for i = 1:length(recession_start)
        if recession_start(i) < max_date && recession_start(i) > min_date
            patch([recession_start(i) min(recession_end(i),max_date) ...
                   min(recession_end(i),max_date) recession_start(i)], ...
                  [y_range(1) y_range(1) y_range(2) y_range(2)], ...
                  [0.8 0.8 0.8], ...   % gray color
                  'EdgeColor','none', ...
                  'FaceAlpha',greytranspar);    % transparency
            hold on
        end
    end

    % Plot GDP growth
    plot(dates(plot_ind),data.gdp_growth(plot_ind),'Linewidth',1.5,'Linestyle','--','color','black')
    hold on
    
    % Plot SPF h-step-ahead forecast
    spf_var = data.("spf_h" + h); 
    plot(dates(plot_ind),spf_var(plot_ind),'Linewidth',1.5,'Linestyle','-','color','#0072BD')

    % Plot SPF h-step-ahead forecast which only exploit current year
    % projections
    spf_var = data.("spf_h" + h + "_fh"); 
    plot(dates(plot_ind),spf_var(plot_ind),'Linewidth',1.5,'Linestyle','-','color','#D95319')
    

    % Range of y-axis
    ylim(y_range)

    % % Legend names specified as strings
    % LegendLocation = 'Southeast';
    % legend('real GDP',"SPF: $h="+h+"$",'interpreter','latex','fontsize',12,'Location',LegendLocation)
    
    % Adjust figure margins
    if ~isempty(plot_ratio)
        set(gcf, 'Units', 'Inches', 'Position', [0, 0, plot_ratio(1), plot_ratio(2)], ...
            'PaperUnits', 'Inches', 'PaperSize', plot_ratio)
    end
    tightfig(hfig);
    box on 

    % Store figures and PNG
    fullFileNamepng = FileName + string(h); 
    saveas(gcf,fullFileNamepng,'png')

end


% Additional legend as .png
hfig = figure;

% Draw empty plot
p1 = plot(NaN,NaN,'Linewidth',1.5,'Linestyle','--','color','black');
hold on
p2 = plot(NaN,NaN,'Linewidth',1.5,'Linestyle','-','color','#0072BD');
p3 = plot(NaN,NaN,'Linewidth',1.5,'Linestyle','-','color','#D95319');

% Remove axis
axis off

% Legend
lgd = legend([p1 p2 p3], {'real GDP','SPF: fixed-event', 'SPF: fixed-horizon'}, ...
    'Interpreter','latex', ...
    'FontSize',12, ...
    'Location','northoutside', ...
    'Orientation','horizontal');

set(gca,'Visible','off')
set(gcf,'Color','white')

% Get legend size in normalized units
lgd.Units = 'inches';
legendPos = lgd.Position;

% Resize figure to legend size (+ small margin)
set(gcf,'Units','inches')
set(gcf,'Position',[1 1 legendPos(3) legendPos(4)])

% Save tightly
tightfig(hfig);
exportgraphics(gcf,'SPF_FH_legend.png','Resolution',300)


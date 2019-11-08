import { Component, DoCheck } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { ScrollToService, ScrollToConfigOptions } from '@nicky-lenaers/ngx-scroll-to';

@Component({
    selector: 'codepeer-history',
    templateUrl: './codepeer-history.component.html',
    styleUrls: [ 'codepeer-history.component.scss' ]
})
export class CodepeerHistoryComponent implements DoCheck {

    private selectedRun: number;

    // Declare variable for charts configuration
    private showXAxis: Boolean = true;
    private showYAxis: Boolean = true;
    private gradient: Boolean = false;
    private showLegend: Boolean = true;
    private showXAxisLabel: Boolean = true;
    private xAxisLabel: String = 'Runs';
    private showYAxisLabel: Boolean = true;
    private yAxisLabel: String = 'Messages';
    private view: any[] = [1300, 600];
    private autoScale: Boolean = true;
    private colorScheme: Object = {
        domain: ['#ff8080', '#ffcb6b', '#ffff4e' , '#AAAAAA']
    };
    private chart_data: any[] = [];

    // Declare variable to limit filter chart_data
    private min_date: Date;
    private max_date: Date;
    private min_run: Number = 1;
    private max_run: Number;

    //Declare variable to get filter data
    private max_run_filter: Number = 0;
    private min_run_filter: Number = 1;
    private max_date_filter: Date;
    private min_date_filter: Date;

    constructor(public reportService: SharedReport,
                 private scrollToService: ScrollToService) {
        this.reportService.page = 'codepeer-history';
    }

    public ngDoCheck() {
        if (this.max_run_filter === 0 && this.reportService.codepeerCurrentRun > 0){
            this.initFilterValues();
            this.buildChartData('id', this.min_run_filter, this.max_run_filter);
        }
    }

    onSelect(event) {
        this.selectedRun = event.name;
        this.goToLine(event.name);
    }

    private initFilterValues() {
        this.max_run_filter = this.reportService.codepeerCurrentRun;
        this.max_run = this.reportService.codepeerCurrentRun;

        this.reportService.codepeer_history.forEach(function(run, idx){
            if (idx === 0){
                this.min_date_filter = run.format_date;
                this.min_date = run.format_date;
            }
            if (idx === this.max_run - 1){
                this.max_date_filter = run.format_date;
                this.max_date = run.format_date;
            }
        }.bind(this))
    }

    private applyRunFilter(min, max){
        this.buildChartData('id', this.min_run_filter, this.max_run_filter);
    }
    private applyDateFilter(min, max){
        this.buildChartData('date', this.min_date_filter, this.max_date_filter);
    }

    private changeSection(section){
        const config: ScrollToConfigOptions = {
            target: section,
            offset: 0,
            duration: 200
        };
        let ret = this.scrollToService.scrollTo(config);
        if (ret.source == undefined){
            console.error("[Error] annotated-source.component:goToLine: scrollToService failed.", ret)
        }
    }

    private goToLine(line: number) {
        if (line) {
            let id = "Run"+line;
            const config: ScrollToConfigOptions = {
                target: id,
                offset: -270,
                duration: 200
            };

            let ret = this.scrollToService.scrollTo(config);
            if (ret.source == undefined){
                console.error("[Error] annotated-source.component:goToLine: scrollToService failed.", ret)
            }
        }
    };

    private buildChartData(type, min, max){
        let new_high = {
            "name": "high",
            "series": []
        };
        let new_medium = {
            "name": "medium",
            "series": []
        };
        let new_low = {
            "name": "low",
            "series": []
        };

        this.reportService.codepeer_history.forEach(
            function(run){
                let comparedValue = (type === 'id' ? run.id : run.format_date);
                if (comparedValue >= min
                    && comparedValue <= max){
                    new_high.series.push(
                        {
                            "name": run.id,
                            "value": run.series[0].value
                        });
                    new_medium.series.push(
                        {
                            "name": run.id,
                            "value": run.series[1].value
                        });
                    new_low.series.push(
                        {
                            "name": run.id,
                            "value": run.series[2].value
                        });
                }
            }.bind(this)
        );

        this.chart_data = [];
        this.chart_data.push(new_high);
        this.chart_data.push(new_medium);
        this.chart_data.push(new_low);
    }

}

import { Component, DoCheck, AfterViewInit, Inject } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { DOCUMENT } from '@angular/common';
import { ScrollToService, ScrollToConfigOptions } from '@nicky-lenaers/ngx-scroll-to';

@Component({
    selector: 'codepeer-history',
    templateUrl: './codepeer-history.component.html',
    styleUrls: [ 'codepeer-history.component.scss' ]
})
export class CodepeerHistoryComponent implements DoCheck, AfterViewInit {

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
    private chartData: any[] = [];

    // Declare variable to limit filter chartData
    private minDate: Date;
    private maxDate: Date;
    private minRun: number = 1;
    private maxRun: number = this.reportService.codepeerCurrentRun;

    // Declare variable to get filter data
    private maxRunFilter: number = 0;
    private minRunFilter: number = 1;
    private maxDateFilter: Date;
    private minDateFilter: Date;

    constructor(public reportService: SharedReport,
                private scrollToService: ScrollToService,
                @Inject(DOCUMENT) private document: Document) {
    }

    public ngAfterViewInit(): void {
        setTimeout(() => {
            this.reportService.setPage('codepeer-history');
        });
    }
    public ngDoCheck(): void {
        if (this.maxRunFilter === 0 && this.reportService.codepeerCurrentRun > 0){
            this.initFilterValues();
            this.buildChartData('id', this.minRunFilter, this.maxRunFilter);
        }
    }

    public onSelect(event: any): void {
        this.selectedRun = event.name;
        this.goToLine(event.name);
    }

    private initFilterValues(): void {
        this.maxRunFilter = this.reportService.codepeerCurrentRun;

        this.reportService.codepeerHistory.forEach(function(run: any, idx: number): void {
            if (idx === 0){
                this.minDateFilter = run.format_date;
                this.minDate = run.format_date;
            }
            if (idx === this.maxRun - 1){
                this.maxDateFilter = run.format_date;
                this.maxDate = run.format_date;
            }
        }.bind(this));
    }

    private applyRunFilter(min: number, max: number): void {
        this.buildChartData('id', this.minRunFilter, this.maxRunFilter);
    }
    private applyDateFilter(min: number, max: number): void {
        this.buildChartData('date', this.minDateFilter, this.maxDateFilter);
    }

    private changeSection(section: string): void {
        const config: ScrollToConfigOptions = {
            target: section,
            offset: 0,
            duration: 200
        };
        let ret: any = this.scrollToService.scrollTo(config);
        if (ret.source === undefined){
            console.error('[Error] annotated-source.component:goToLine:'
                          + ' scrollToService failed.', ret);
        }
    }

    private goToLine(line: number): void {
        if (line) {
            let id: string = 'Run' + line;
            try {
                let elem: HTMLElement = this.document.getElementById(id);
                elem.scrollIntoView({block: 'center', inline: 'nearest'});
            } catch (err) {
                console.warn(err);
            }
        }
    };

    private buildChartData(type: string, min: any, max: any): void {
        let newHigh: any = {
            name: 'high',
            series: []
        };
        let newMedium: any = {
            name: 'medium',
            series: []
        };
        let newLow: any = {
            name: 'low',
            series: []
        };

        this.reportService.codepeerHistory.forEach(
            function(run: any): void {
                let comparedValue: number = (type === 'id' ? run.id : run.format_date);
                if (comparedValue >= min
                    && comparedValue <= max){
                    newHigh.series.push(
                        {
                            name: run.id,
                            value: run.series[0].value
                        });
                    newMedium.series.push(
                        {
                            name: run.id,
                            value: run.series[1].value
                        });
                    newLow.series.push(
                        {
                            name: run.id,
                            value: run.series[2].value
                        });
                }
            }.bind(this)
        );

        this.chartData = [];
        this.chartData.push(newHigh);
        this.chartData.push(newMedium);
        this.chartData.push(newLow);
    }

}

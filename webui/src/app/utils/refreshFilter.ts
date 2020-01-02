import { IProperty, IMessage, ICountRanking, ISourceNav,
         ISource, IModule, IReviewFilter, IToolFilter,
         ISourceDir, IRankingFilter, IPropertyFilter,
         IRuleFilter, IFilterIndex } from 'gnat';

function incMessageCount(id: number, array: any): void {
    array.forEach(function(cell: any): void {
        if (cell.id === id){
            cell._ui_selected_message_count += 1;
        }
    });
}

function incRevMessageCount(name: string, array: any): void {
    if (array){
        array.forEach(function(cell: any): void {
            if (cell.name.toUpperCase() === name.toUpperCase()){
                cell._ui_selected_message_count += 1;
            }
        });
    }
}

function isSelected(id: number, array: any): boolean {
    let isSelected: boolean;

    array.forEach(function(cell: any): void {
        if (cell.id === id){
            if (cell._ui_unselected) {
                isSelected = !cell._ui_unselected;
            } else {
                isSelected = true;
            }
        }
    });
    return isSelected;
}

function reviewSelected(name: string, array: any): boolean {
    let isSelected: boolean;
    if (array){
        array.forEach(function(cell: any): void {
            if (cell.name.toUpperCase() === name.toUpperCase()){
                if (cell._ui_unselected) {
                    isSelected = !cell._ui_unselected;
                } else {
                    isSelected = true;
                }
            }
        });
    } else {
        isSelected = true;
    }
    return isSelected;
}

function initCountRanking(): ICountRanking {
    return {
        High: 0,
        Medium: 0,
        Low: 0,
        Info: 0,
        Unspecified : 0
    };
}

export function updateFilter(reportService: any): void {
    const tools: [ IToolFilter ] = reportService.filter.tools;
    const rules: [ IRuleFilter ] = reportService.filter.rules;
    const properties: [ IPropertyFilter ] = reportService.filter.properties;
    const ranking: [ IRankingFilter ] = reportService.filter.ranking;
    const review: [ IReviewFilter ] = reportService.filter.review_status;
    reportService.totalMessageCount = 0;

    if (tools){
        tools.forEach(function(tool: IToolFilter): void {
            tool._ui_selected_message_count = 0;
        });
    }

    if (tools){
        rules.forEach(function(rule: IRuleFilter): void {
            rule._ui_selected_message_count = 0;
        });
    }

    if (properties){
        properties.forEach(function(property: IPropertyFilter): void {
            property._ui_selected_message_count = 0;
        });
    }

    if (ranking){
        ranking.forEach(function(rank: IRankingFilter): void {
            rank._ui_selected_message_count = 0;
        });
    }

    if (review){
        review.forEach(function(rev: IReviewFilter): void {
            rev._ui_selected_message_count = 0;
        });
    }

    if (reportService && reportService.message && reportService.message.sources){
        reportService.message.sources.forEach(function(source: ISourceNav): void {
            source._ui_total_message_count = 0;
        });
    }

    reportService.code.modules.forEach(function(myModule: IModule): void {
        myModule._ui_total_message_count = 0;

        myModule.source_dirs.forEach(function(folder: ISourceDir): void {
            folder._ui_total_message_count = 0;

            folder.sources.forEach(function(codeSource: ISource): void {
                codeSource._ui_total_message_count = 0;

                reportService.message.sources.forEach(function(source: ISourceNav): void {

                    if (!source.countRanking) {
                        source.countRanking = initCountRanking();
                    }
                    if (!source._ui_total_message_count){
                        source._ui_total_message_count = 0;
                    }

                    if (source.filename.toUpperCase() === codeSource.filename.toUpperCase() &&
                        source.messages != null){
                        source.countRanking = initCountRanking();
                        source._ui_total_message_count = 0;

                        source.messages.forEach(function(message: IMessage): void {
                            const toolId: number = message.rule.tool_id;
                            const ruleId: number = message.rule.id;
                            const rankId: number = message.ranking.id;
                            const reviewName: string = (message.user_review ?
                                                        message.user_review.status :
                                                        'UNCATEGORIZED');

                            const isToolSelected: boolean = isSelected(toolId, tools);
                            const isRuleSelected: boolean = isSelected(ruleId, rules);
                            const isRankSelected: boolean = isSelected(rankId, ranking);
                            const IsReviewSelected: boolean = reviewSelected(reviewName, review);

                            let hasSelectedProperties: boolean = false;

                            if (message.properties != null){
                                if (message.properties.length < 1){
                                    hasSelectedProperties = true;
                                }
                                message.properties.forEach(function(property: IProperty): void {
                                    let isPropertySelected: boolean =
                                        isSelected(property.id, properties);
                                    if (isPropertySelected){
                                        hasSelectedProperties = true;
                                    }
                                }.bind(this));
                            }

                            if (isToolSelected && isRuleSelected && isRankSelected
                                && IsReviewSelected && hasSelectedProperties) {
                                incMessageCount(toolId, tools);
                                incMessageCount(ruleId, rules);
                                incMessageCount(rankId, ranking);
                                incRevMessageCount(reviewName, review);
                                message.properties.forEach(function(property: IProperty): void {
                                    let isPropertySelected: boolean =
                                        isSelected(property.id, properties);
                                    if (isPropertySelected){
                                        incMessageCount(property.id, properties);
                                    }
                                }.bind(this));

                                source.countRanking[message.ranking.name] += 1;

                                message.hide = true;
                                source._ui_total_message_count ++;
                                codeSource._ui_total_message_count ++;
                                folder._ui_total_message_count++;
                                myModule._ui_total_message_count++;
                                reportService.totalMessageCount ++;
                            } else {
                                message.hide = false;
                            }
                        }.bind(this));
                    }
                }.bind(this));
            }.bind(this));
        }.bind(this));
    }.bind(this));
}

function incMessageCount(id: number, array: any){
    array.forEach(function(cell){
        if (cell.id === id){
            cell._ui_selected_message_count += 1;
        }
    });
}

function incRevMessageCount(name: string, array: any){
    if (array){
        array.forEach(function(cell){
            if (cell.name === name){
                cell._ui_selected_message_count += 1;
            }
        });
    }
}

function isSelected(id: number, array: any): boolean {
    let isSelected: boolean;

    array.forEach(function(cell){
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
        array.forEach(function(cell){
            if (cell.name === name){
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

export function updateFilter(reportService) {
    const tools = reportService.filter.tools;
    const rules = reportService.filter.rules;
    const properties = reportService.filter.properties;
    const ranking = reportService.filter.ranking;
    const review = reportService.filter.review_status;
    reportService._ui_total_message_count = 0;

    if (tools){
        tools.forEach(function(tool){
            tool._ui_selected_message_count = 0;
        });
    }

    if (tools){
        rules.forEach(function(rule){
            rule._ui_selected_message_count = 0;
        });
    }

    if (properties){
        properties.forEach(function(property){
            property._ui_selected_message_count = 0;
        });
    }

    if (ranking){
        ranking.forEach(function(rank){
            rank._ui_selected_message_count = 0;
        });
    }

    if (review){
        review.forEach(function(rev){
            rev._ui_selected_message_count = 0;
        });
    }

    if (reportService && reportService.message && reportService.message.sources){
        reportService.message.sources.forEach(function(source){
            source._ui_total_message_count;
        });
    }

    reportService.code.modules.forEach(function(myModule){
        myModule._ui_total_message_count = 0;

        myModule.source_dirs.forEach(function(folder){
            folder._ui_total_message_count = 0;

            folder.sources.forEach(function(codeSource){
                codeSource._ui_total_message_count = 0;

                reportService.message.sources.forEach(function(source){
                    if (source.filename === codeSource.filename &&
                        source.messages != null){
                        source._ui_total_message_count = 0;

                        source.messages.forEach(function(message){
                            const toolId = message.rule.tool_id;
                            const ruleId = message.rule.id;
                            const rankId = message.ranking.id;
                            const reviewName = (message.user_review ? message.user_review.status : 'UNCATEGORIZED');

                            const isToolSelected = isSelected(toolId, tools);
                            const isRuleSelected = isSelected(ruleId, rules);
                            const isRankSelected = isSelected(rankId, ranking);
                            const IsReviewSelected = reviewSelected(reviewName, review);

                            let hasSelectedProperties = false;

                            if (message.properties != null){
                                if (message.properties.length === 0){
                                    hasSelectedProperties = true;
                                }
                                message.properties.forEach(function(property){
                                    let isPropertySelected =
                                        isSelected(property.id, properties);
                                    if (isPropertySelected){
                                        hasSelectedProperties = true;
                                    }
                                }.bind(this));
                            }

                            if (isToolSelected && isRuleSelected && isRankSelected && IsReviewSelected && hasSelectedProperties) {
                                incMessageCount(toolId, tools);
                                incMessageCount(ruleId, rules);
                                incMessageCount(rankId, ranking);
                                incRevMessageCount(reviewName, review);
                                message.properties.forEach(function(property){
                                    let isPropertySelected =
                                        isSelected(property.id, properties);
                                    if (isPropertySelected){
                                        incMessageCount(property.id, properties);
                                    }
                                }.bind(this));

                                message.hide = true;
                                source._ui_total_message_count ++;
                                codeSource._ui_total_message_count ++;
                                folder._ui_total_message_count++;
                                myModule._ui_total_message_count++;
                                reportService._ui_total_message_count ++;
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

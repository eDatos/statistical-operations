package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.client.view.handlers.ExternalUiHandlers;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.actions.SearchPaginatedAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.SearchExternalItemLinkItem;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

public class SearchItemItem extends SearchExternalItemLinkItem {

    private ExternalUiHandlers                                        uiHandlers;
    protected SearchExternalItemPaginatedWithExternalItemFilterWindow searchItemWindow;

    private ClickHandler                                              saveClickHandler;

    public SearchItemItem(final String name, String title, final TypeExternalArtefactsEnum itemSchemeType, final TypeExternalArtefactsEnum itemType, final String windowTitle,
            final String windowFilterListTitle, final String windowFilterTextTitle, final String windowSelectionListTitle) {
        super(name, title);

        getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

            @Override
            public void onFormItemClick(FormItemIconClickEvent event) {
                final int FIRST_RESULT = 0;
                final int MAX_RESULTS = 6;
                PaginatedAction filterListAction = new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        getUiHandlers().retrieveItemSchemes(name, itemSchemeType, firstResult, maxResults, searchItemWindow.getFilterListCriteria());
                    }
                };
                PaginatedAction selectionListAction = new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        getUiHandlers().retrieveItems(name, itemType, firstResult, maxResults, searchItemWindow.getSelectionListCriteria(), searchItemWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                };
                searchItemWindow = new SearchExternalItemPaginatedWithExternalItemFilterWindow(windowTitle, windowFilterListTitle, windowFilterTextTitle, windowSelectionListTitle, MAX_RESULTS,
                        filterListAction, selectionListAction);

                // Load the list of items (to populate the selection window)
                getUiHandlers().retrieveItemSchemes(name, itemSchemeType, FIRST_RESULT, MAX_RESULTS, null);
                getUiHandlers().retrieveItems(name, itemType, FIRST_RESULT, MAX_RESULTS, null, null);

                // Filter items when the item scheme filter changes
                searchItemWindow.getFilterListItem().getListGrid().addRecordClickHandler(new RecordClickHandler() {

                    @Override
                    public void onRecordClick(RecordClickEvent event) {
                        getUiHandlers().retrieveItems(name, itemType, FIRST_RESULT, MAX_RESULTS, searchItemWindow.getSelectionListCriteria(), searchItemWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                });
                searchItemWindow.getClearButton().addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

                    @Override
                    public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                        getUiHandlers().retrieveItems(name, itemType, FIRST_RESULT, MAX_RESULTS, searchItemWindow.getSelectionListCriteria(), searchItemWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                });

                // Set the search actions
                searchItemWindow.setSelectionListSearchAction(new SearchPaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, String criteria) {
                        getUiHandlers().retrieveItems(name, itemType, firstResult, maxResults, criteria, searchItemWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                });
                searchItemWindow.setFilterListSearchAction(new SearchPaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, String criteria) {
                        getUiHandlers().retrieveItemSchemes(name, itemSchemeType, firstResult, maxResults, criteria);
                    }
                });

                searchItemWindow.getSave().addClickHandler(saveClickHandler);

            }
        });
    }

    public void setItemSchemes(ExternalItemsResult result) {
        if (searchItemWindow != null) {
            searchItemWindow.setFilterRelatedResources(result.getExternalItemDtos());
            searchItemWindow.refreshFilterListPaginationInfo(result.getFirstResult(), result.getExternalItemDtos().size(), result.getTotalResults());
        }
    }

    public void setItems(ExternalItemsResult result) {
        if (searchItemWindow != null) {
            searchItemWindow.setRelatedResources(result.getExternalItemDtos());
            searchItemWindow.refreshListPaginationInfo(result.getFirstResult(), result.getExternalItemDtos().size(), result.getTotalResults());
        }
    }

    public void markSearchWindowForDestroy() {
        searchItemWindow.markForDestroy();
    }

    public void setSaveClickHandler(ClickHandler clickHandler) {
        this.saveClickHandler = clickHandler;
    }

    public void setUiHandlers(ExternalUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }

    public ExternalUiHandlers getUiHandlers() {
        return uiHandlers;
    }

    public SearchExternalItemPaginatedWithExternalItemFilterWindow getSearchWindow() {
        return searchItemWindow;
    }

    public ExternalItemDto getSelectedItem() {
        if (searchItemWindow != null) {
            return searchItemWindow.getSelectedRelatedResource();
        }
        return null;
    }
}

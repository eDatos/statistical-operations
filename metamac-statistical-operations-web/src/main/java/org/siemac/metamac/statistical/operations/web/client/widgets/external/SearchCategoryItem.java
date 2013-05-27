package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
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

public class SearchCategoryItem extends SearchExternalItemLinkItem {

    private ExternalUiHandlers     uiHandlers;
    protected SearchCategoryWindow searchCategoryWindow;

    private ClickHandler           saveClickHandler;

    public SearchCategoryItem(final String name, String title) {
        super(name, title);

        getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

            @Override
            public void onFormItemClick(FormItemIconClickEvent event) {
                final int FIRST_RESULT = 0;
                final int MAX_RESULTS = 6;
                PaginatedAction filterListAction = new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        getUiHandlers().retrieveCategorySchemes(name, firstResult, maxResults, searchCategoryWindow.getFilterListCriteria());
                    }
                };
                PaginatedAction selectionListAction = new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        getUiHandlers()
                                .retrieveCategories(name, firstResult, maxResults, searchCategoryWindow.getSelectionListCriteria(), searchCategoryWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                };
                searchCategoryWindow = new SearchCategoryWindow(MAX_RESULTS, filterListAction, selectionListAction);

                // Load the list of categories (to populate the selection window)
                getUiHandlers().retrieveCategorySchemes(name, FIRST_RESULT, MAX_RESULTS, null);
                getUiHandlers().retrieveCategories(name, FIRST_RESULT, MAX_RESULTS, null, null);

                // Filter categories when the category scheme filter changes
                searchCategoryWindow.getFilterListItem().getListGrid().addRecordClickHandler(new RecordClickHandler() {

                    @Override
                    public void onRecordClick(RecordClickEvent event) {
                        getUiHandlers().retrieveCategories(name, FIRST_RESULT, MAX_RESULTS, searchCategoryWindow.getSelectionListCriteria(),
                                searchCategoryWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                });
                searchCategoryWindow.getClearButton().addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

                    @Override
                    public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                        getUiHandlers().retrieveCategories(name, FIRST_RESULT, MAX_RESULTS, searchCategoryWindow.getSelectionListCriteria(),
                                searchCategoryWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                });

                // Set the search actions
                searchCategoryWindow.setSelectionListSearchAction(new SearchPaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, String criteria) {
                        getUiHandlers().retrieveCategories(name, firstResult, maxResults, criteria, searchCategoryWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                });
                searchCategoryWindow.setFilterListSearchAction(new SearchPaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, String criteria) {
                        getUiHandlers().retrieveCategorySchemes(name, firstResult, maxResults, criteria);
                    }
                });

                searchCategoryWindow.getSave().addClickHandler(saveClickHandler);

            }
        });
    }

    public void setCategorySchemes(ExternalItemsResult result) {
        if (searchCategoryWindow != null) {
            searchCategoryWindow.setFilterRelatedResources(result.getExternalItemDtos());
            searchCategoryWindow.refreshFilterListPaginationInfo(result.getFirstResult(), result.getExternalItemDtos().size(), result.getTotalResults());
        }
    }

    public void setCategories(ExternalItemsResult result) {
        if (searchCategoryWindow != null) {
            searchCategoryWindow.setRelatedResources(result.getExternalItemDtos());
            searchCategoryWindow.refreshListPaginationInfo(result.getFirstResult(), result.getExternalItemDtos().size(), result.getTotalResults());
        }
    }

    public void markSearchWindowForDestroy() {
        searchCategoryWindow.markForDestroy();
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

    public SearchCategoryWindow getSearchCategoryWindow() {
        return searchCategoryWindow;
    }

    public ExternalItemDto getSelectedCategory() {
        if (searchCategoryWindow != null) {
            return searchCategoryWindow.getSelectedRelatedResource();
        }
        return null;
    }
}

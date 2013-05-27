package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.statistical.operations.web.client.view.handlers.ExternalUiHandlers;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.actions.SearchPaginatedAction;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

public class SearchMultipleCategoriesItem extends ExternalItemListItem {

    private ExternalUiHandlers               uiHandlers;
    protected SearchMultipleCategoriesWindow searchMultipleCategoriesWindow;

    private ClickHandler                     saveClickHandler;

    public SearchMultipleCategoriesItem(final String name, String title) {
        super(name, title, true);

        getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

            @Override
            public void onFormItemClick(FormItemIconClickEvent event) {
                final int FIRST_RESULT = 0;
                final int MAX_RESULTS = 6;
                PaginatedAction filterListAction = new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        getUiHandlers().retrieveCategorySchemes(name, firstResult, maxResults, searchMultipleCategoriesWindow.getFilterListCriteria());
                    }
                };
                PaginatedAction selectionListAction = new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        getUiHandlers().retrieveCategories(name, firstResult, maxResults, searchMultipleCategoriesWindow.getSelectionListCriteria(),
                                searchMultipleCategoriesWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                };
                searchMultipleCategoriesWindow = new SearchMultipleCategoriesWindow(MAX_RESULTS, filterListAction, selectionListAction);

                // Load the list of categories (to populate the selection window)
                getUiHandlers().retrieveCategorySchemes(name, FIRST_RESULT, MAX_RESULTS, null);
                getUiHandlers().retrieveCategories(name, FIRST_RESULT, MAX_RESULTS, null, null);

                // Filter categories when the category scheme filter changes
                searchMultipleCategoriesWindow.getFilterListItem().getListGrid().addRecordClickHandler(new RecordClickHandler() {

                    @Override
                    public void onRecordClick(RecordClickEvent event) {
                        getUiHandlers().retrieveCategories(name, FIRST_RESULT, MAX_RESULTS, searchMultipleCategoriesWindow.getSelectionListCriteria(),
                                searchMultipleCategoriesWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                });
                searchMultipleCategoriesWindow.getClearButton().addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

                    @Override
                    public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                        getUiHandlers().retrieveCategories(name, FIRST_RESULT, MAX_RESULTS, searchMultipleCategoriesWindow.getSelectionListCriteria(),
                                searchMultipleCategoriesWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                });

                // Set the search actions
                searchMultipleCategoriesWindow.setSelectionListSearchAction(new SearchPaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, String criteria) {
                        getUiHandlers().retrieveCategories(name, firstResult, maxResults, criteria, searchMultipleCategoriesWindow.getSelectedRelatedResourceUrnAsFilter());
                    }
                });
                searchMultipleCategoriesWindow.setFilterListSearchAction(new SearchPaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, String criteria) {
                        getUiHandlers().retrieveCategorySchemes(name, firstResult, maxResults, criteria);
                    }
                });

                searchMultipleCategoriesWindow.getSave().addClickHandler(saveClickHandler);
            }
        });
    }

    public void setCategorySchemes(ExternalItemsResult result) {
        if (searchMultipleCategoriesWindow != null) {
            searchMultipleCategoriesWindow.setFilterRelatedResources(result.getExternalItemDtos());
            searchMultipleCategoriesWindow.refreshFilterListPaginationInfo(result.getFirstResult(), result.getExternalItemDtos().size(), result.getTotalResults());
        }
    }

    public void setCategories(ExternalItemsResult result) {
        if (searchMultipleCategoriesWindow != null) {
            searchMultipleCategoriesWindow.setSourceRelatedResources(result.getExternalItemDtos());
            searchMultipleCategoriesWindow.refreshSourcePaginationInfo(result.getFirstResult(), result.getExternalItemDtos().size(), result.getTotalResults());
        }
    }

    public void markSearchWindowForDestroy() {
        searchMultipleCategoriesWindow.markForDestroy();
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

    public SearchMultipleCategoriesWindow getSearchCategoriesWindow() {
        return searchMultipleCategoriesWindow;
    }

    public List<ExternalItemDto> getSelectedCategories() {
        if (searchMultipleCategoriesWindow != null) {
            return searchMultipleCategoriesWindow.getSelectedRelatedResources();
        }
        return new ArrayList<ExternalItemDto>();
    }
}

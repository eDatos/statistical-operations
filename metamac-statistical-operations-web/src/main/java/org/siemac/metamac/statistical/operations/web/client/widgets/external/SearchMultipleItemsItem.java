package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.statistical.operations.web.client.view.handlers.ExternalResourcesUiHandlers;
import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.actions.SearchPaginatedAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomSelectItem;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

public class SearchMultipleItemsItem extends ExternalItemListItem {

    protected final int                                                       FIRST_RESULT = 0;
    protected final int                                                       MAX_RESULTS  = 6;

    protected SearchMultipleExternalItemPaginatedWithExternalItemFilterWindow searchMultipleItemsWindow;
    protected ExternalResourcesUiHandlers                                     uiHandlers;
    protected ClickHandler                                                    saveClickHandler;

    protected String                                                          defaultItemSchemeUrn;

    protected CustomSelectItem                                                initialFilterSelectItem;

    public SearchMultipleItemsItem(final String name, String title, final ExternalResourceWebCriteria itemSchemeCriteria, final ItemWebCriteria itemCriteria, final String windowTitle,
            final String windowFilterListTitle, final String windowFilterTextTitle, final String windowSelectionListTitle, final MultipleExternalResourceAction action) {
        this(name, title, itemSchemeCriteria, itemCriteria, windowTitle, windowFilterListTitle, windowFilterTextTitle, windowSelectionListTitle, null, action);
    }

    public SearchMultipleItemsItem(final String name, String title, final ExternalResourceWebCriteria itemSchemeCriteria, final ItemWebCriteria itemCriteria, final String windowTitle,
            final String windowFilterListTitle, final String windowFilterTextTitle, final String windowSelectionListTitle, final CustomSelectItem initialFilterSelectItem,
            final MultipleExternalResourceAction action) {
        super(name, title, true);
        this.initialFilterSelectItem = initialFilterSelectItem;

        getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

            @Override
            public void onFormItemClick(FormItemIconClickEvent event) {

                PaginatedAction filterListAction = new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        itemSchemeCriteria.setCriteria(searchMultipleItemsWindow.getFilterListCriteria());
                        retrieveItemSchemes(name, itemSchemeCriteria, firstResult, maxResults);
                    }
                };
                PaginatedAction selectionListAction = new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        itemCriteria.setCriteria(searchMultipleItemsWindow.getSelectionListCriteria());
                        itemCriteria.setItemSchemUrn(searchMultipleItemsWindow.getSelectedRelatedResourceUrnAsFilter());
                        retrieveItems(name, itemCriteria, firstResult, maxResults);
                    }
                };
                searchMultipleItemsWindow = new SearchMultipleExternalItemPaginatedWithExternalItemFilterWindow(windowTitle, windowFilterListTitle, windowFilterTextTitle, windowSelectionListTitle,
                        MAX_RESULTS, filterListAction, selectionListAction);

                // Add the selectItem in the initial filter form (if has been specified!)
                if (initialFilterSelectItem != null) {
                    searchMultipleItemsWindow.getInitialFilterForm().setFields(initialFilterSelectItem);
                }

                // ----------------------------------------------------------------------------

                // Set the default itemScheme
                searchMultipleItemsWindow.setFilterTextValue(defaultItemSchemeUrn);
                searchMultipleItemsWindow.setTextInFilterSearchSection(defaultItemSchemeUrn);
                itemSchemeCriteria.setCriteria(defaultItemSchemeUrn);
                itemCriteria.setCriteria(null);
                itemCriteria.setItemSchemUrn(defaultItemSchemeUrn);

                // ----------------------------------------------------------------------------

                // Load the list of item schemes (to populate the selection window)
                retrieveItemSchemes(name, itemSchemeCriteria, FIRST_RESULT, MAX_RESULTS);
                retrieveItems(name, itemCriteria, FIRST_RESULT, MAX_RESULTS);

                // Set external items previously selected
                searchMultipleItemsWindow.setTargetRelatedResources(action.getExternalItemsPreviouslySelected());

                // Filter items when the item scheme filter changes
                searchMultipleItemsWindow.getFilterListItem().getListGrid().addRecordClickHandler(new RecordClickHandler() {

                    @Override
                    public void onRecordClick(RecordClickEvent event) {
                        itemCriteria.setCriteria(searchMultipleItemsWindow.getSelectionListCriteria());
                        itemCriteria.setItemSchemUrn(searchMultipleItemsWindow.getSelectedRelatedResourceUrnAsFilter());
                        retrieveItems(name, itemCriteria, FIRST_RESULT, MAX_RESULTS);
                    }
                });
                searchMultipleItemsWindow.getClearButton().addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

                    @Override
                    public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                        itemCriteria.setCriteria(searchMultipleItemsWindow.getSelectionListCriteria());
                        itemCriteria.setItemSchemUrn(searchMultipleItemsWindow.getSelectedRelatedResourceUrnAsFilter());
                        retrieveItems(name, itemCriteria, FIRST_RESULT, MAX_RESULTS);
                    }
                });

                // Set the search actions
                searchMultipleItemsWindow.setSelectionListSearchAction(new SearchPaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, String criteria) {
                        itemCriteria.setCriteria(criteria);
                        itemCriteria.setItemSchemUrn(searchMultipleItemsWindow.getSelectedRelatedResourceUrnAsFilter());
                        retrieveItems(name, itemCriteria, firstResult, maxResults);
                    }
                });
                searchMultipleItemsWindow.setFilterListSearchAction(new SearchPaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, String criteria) {
                        itemSchemeCriteria.setCriteria(criteria);
                        retrieveItemSchemes(name, itemSchemeCriteria, firstResult, maxResults);
                    }
                });

                searchMultipleItemsWindow.getSave().addClickHandler(saveClickHandler);
            }
        });
    }

    protected void retrieveItemSchemes(String formItemName, ExternalResourceWebCriteria externalResourceWebCriteria, int firstResult, int maxResults) {
        getUiHandlers().retrieveItemSchemes(formItemName, externalResourceWebCriteria, firstResult, maxResults);
    }

    protected void retrieveItems(String formItemName, ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) {
        getUiHandlers().retrieveItems(formItemName, itemWebCriteria, firstResult, maxResults);
    }

    public void setItemSchemes(ExternalItemsResult result) {
        if (searchMultipleItemsWindow != null) {
            searchMultipleItemsWindow.setFilterRelatedResources(result.getExternalItemDtos());
            searchMultipleItemsWindow.refreshFilterListPaginationInfo(result.getFirstResult(), result.getExternalItemDtos().size(), result.getTotalResults());
        }
    }

    public void setItems(ExternalItemsResult result) {
        if (searchMultipleItemsWindow != null) {
            searchMultipleItemsWindow.setSourceRelatedResources(result.getExternalItemDtos());
            searchMultipleItemsWindow.refreshSourcePaginationInfo(result.getFirstResult(), result.getExternalItemDtos().size(), result.getTotalResults());
        }
    }

    public void markSearchWindowForDestroy() {
        searchMultipleItemsWindow.markForDestroy();
    }

    public void setSaveClickHandler(ClickHandler clickHandler) {
        this.saveClickHandler = clickHandler;
    }

    public void setUiHandlers(ExternalResourcesUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }

    public ExternalResourcesUiHandlers getUiHandlers() {
        return uiHandlers;
    }

    public SearchMultipleExternalItemPaginatedWithExternalItemFilterWindow getSearchWindow() {
        return searchMultipleItemsWindow;
    }

    public List<ExternalItemDto> getSelectedItems() {
        if (searchMultipleItemsWindow != null) {
            return searchMultipleItemsWindow.getSelectedRelatedResources();
        }
        return new ArrayList<ExternalItemDto>();
    }

    public void setDefaultItemSchemeUrn(String defaultItemSchemeUrn) {
        this.defaultItemSchemeUrn = defaultItemSchemeUrn;
    }

    public CustomSelectItem getInitialFilterSelectItem() {
        return initialFilterSelectItem;
    }
}

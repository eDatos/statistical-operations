package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.web.common.client.view.handlers.SrmExternalResourcesUiHandlers;
import org.siemac.metamac.web.common.client.widgets.SearchMultipleExternalItemWindow;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.shared.criteria.SrmItemSchemeRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;

public class SearchMultipleItemSchemesItem extends ExternalItemListItem {

    private final int                        FIRST_RESULT = 0;
    private final int                        MAX_RESULTS  = 6;

    private SearchMultipleExternalItemWindow searchMultipleItemSchemesWindow;

    private SrmExternalResourcesUiHandlers   uiHandlers;

    private ClickHandler                     saveClickHandler;

    public SearchMultipleItemSchemesItem(final String name, String title, final MultipleExternalResourceAction action) {
        super(name, title, true);

        getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

            @Override
            public void onFormItemClick(FormItemIconClickEvent event) {

                final SrmItemSchemeRestCriteria criteria = new SrmItemSchemeRestCriteria(TypeExternalArtefactsEnum.CODELIST);

                searchMultipleItemSchemesWindow = new SearchMultipleExternalItemWindow(getConstants().searchCodelists(), MAX_RESULTS, new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        criteria.setCriteria(searchMultipleItemSchemesWindow.getSearchCriteria());
                        getUiHandlers().retrieveItemSchemes(name, criteria, firstResult, maxResults);
                    }
                });

                getUiHandlers().retrieveItemSchemes(name, criteria, FIRST_RESULT, MAX_RESULTS);

                searchMultipleItemSchemesWindow.setSelectedExternalItems(action.getExternalItemsPreviouslySelected());

                searchMultipleItemSchemesWindow.getSave().addClickHandler(saveClickHandler);
            }
        });
    }

    public SrmExternalResourcesUiHandlers getUiHandlers() {
        return uiHandlers;
    }

    public void setUiHandlers(SrmExternalResourcesUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }

    public void setSourceExternalItems(ExternalItemsResult result) {
        if (searchMultipleItemSchemesWindow != null) {
            searchMultipleItemSchemesWindow.setSourceExternalItems(result.getExternalItemDtos());
            searchMultipleItemSchemesWindow.refreshSourcePaginationInfo(result.getFirstResult(), result.getExternalItemDtos().size(), result.getTotalResults());
        }
    }

    public void setTargetExternalItems(List<ExternalItemDto> externalItemDtos) {
        if (searchMultipleItemSchemesWindow != null) {
            searchMultipleItemSchemesWindow.setSelectedExternalItems(externalItemDtos);
        }
    }

    public List<ExternalItemDto> getSelectedItemSchemes() {
        return searchMultipleItemSchemesWindow.getSelectedExternalItems();
    }

    public void markSearchWindowForDestroy() {
        searchMultipleItemSchemesWindow.markForDestroy();
    }

    public void setSaveClickHandler(ClickHandler clickHandler) {
        this.saveClickHandler = clickHandler;
    }
}

package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.web.common.client.MetamacWebCommon;
import org.siemac.metamac.web.common.client.view.handlers.SrmExternalResourcesUiHandlers;
import org.siemac.metamac.web.common.client.widgets.SearchMultipleExternalItemWindow;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.actions.SearchPaginatedAction;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomCheckboxItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.ExternalItemListItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.MultipleExternalResourceAction;
import org.siemac.metamac.web.common.shared.criteria.SrmExternalResourceRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;

public class SearchMultipleSrmItemSchemesItem extends ExternalItemListItem {

    private final int                        FIRST_RESULT = 0;
    private final int                        MAX_RESULTS  = 6;

    private SearchMultipleExternalItemWindow searchMultipleItemSchemesWindow;

    protected CustomCheckboxItem             isLastVersionItem;

    private SrmExternalResourcesUiHandlers   uiHandlers;

    private ClickHandler                     saveClickHandler;

    public SearchMultipleSrmItemSchemesItem(final String name, String title, final MultipleExternalResourceAction action) {
        super(name, title, true);

        getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

            @Override
            public void onFormItemClick(FormItemIconClickEvent event) {

                final SrmExternalResourceRestCriteria itemSchemeRestcriteria = new SrmExternalResourceRestCriteria(TypeExternalArtefactsEnum.CODELIST);

                searchMultipleItemSchemesWindow = new SearchMultipleExternalItemWindow(getConstants().searchCodelists(), MAX_RESULTS, new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        retrieveItemSchemes(name, itemSchemeRestcriteria, firstResult, maxResults);
                    }
                });
                searchMultipleItemSchemesWindow.setSearchAction(new SearchPaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, String criteria) {
                        retrieveItemSchemes(name, itemSchemeRestcriteria, firstResult, maxResults);
                    }
                });

                // Add isLastVersion item
                CustomDynamicForm filterForm = new CustomDynamicForm();
                filterForm.setColWidths("30%", "70%");
                filterForm.setWidth(searchMultipleItemSchemesWindow.getWidth());
                filterForm.setStyleName("marginTop15");
                isLastVersionItem = new CustomCheckboxItem("is-last-version", MetamacWebCommon.getConstants().resourceOnlyLastVersion());
                isLastVersionItem.setTitleStyle("staticFormItemTitle");
                isLastVersionItem.addChangedHandler(new ChangedHandler() {

                    @Override
                    public void onChanged(ChangedEvent event) {
                        retrieveItemSchemes(name, itemSchemeRestcriteria, FIRST_RESULT, MAX_RESULTS);
                    }
                });
                filterForm.setFields(isLastVersionItem);
                searchMultipleItemSchemesWindow.getSearchWindowLayout().addMember(filterForm, 0);

                retrieveItemSchemes(name, itemSchemeRestcriteria, FIRST_RESULT, MAX_RESULTS);

                searchMultipleItemSchemesWindow.setSelectedExternalItems(action.getExternalItemsPreviouslySelected());
                searchMultipleItemSchemesWindow.getSave().addClickHandler(saveClickHandler);
            }
        });
    }

    private void retrieveItemSchemes(String formItemName, SrmExternalResourceRestCriteria srmItemSchemeRestCriteria, int firstResult, int maxResults) {
        srmItemSchemeRestCriteria.setCriteria(searchMultipleItemSchemesWindow.getSearchCriteria());
        srmItemSchemeRestCriteria.setOnlyLastVersion(isLastVersionItem.getValueAsBoolean());
        getUiHandlers().retrieveItemSchemes(formItemName, srmItemSchemeRestCriteria, firstResult, maxResults);
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

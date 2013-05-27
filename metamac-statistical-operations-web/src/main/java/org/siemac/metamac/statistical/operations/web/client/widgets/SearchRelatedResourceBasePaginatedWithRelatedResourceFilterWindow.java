package org.siemac.metamac.statistical.operations.web.client.widgets;

import java.util.List;

import org.siemac.metamac.core.common.dto.RelatedResourceBaseDto;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.web.common.client.MetamacWebCommon;
import org.siemac.metamac.web.common.client.widgets.CustomWindow;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.actions.SearchPaginatedAction;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomButtonItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.SearchRelatedResourceBasePaginatedDragAndDropItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.SearchRelatedResourceBasePaginatedItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;
import org.siemac.metamac.web.common.shared.RelatedResourceBaseUtils;

import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

/**
 * Window with a {@link SearchRelatedResourceBasePaginatedDragAndDropItem} and a {@link SearchRelatedResourceBasePaginatedItem} as a filter.
 * 
 * @param <T>
 */
public class SearchRelatedResourceBasePaginatedWithRelatedResourceFilterWindow<T extends RelatedResourceBaseDto> extends CustomWindow {

    protected static final String                       FORM_ITEM_CUSTOM_WIDTH = "500";

    protected static final String                       FIELD_SAVE             = "save-ex";

    protected CustomDynamicForm                         filterForm;
    protected CustomDynamicForm                         form;

    protected SearchRelatedResourceBasePaginatedItem<T> filterListItem;
    protected ViewTextItem                              filterTextItem;
    protected CustomButtonItem                          clearButton;

    protected SearchRelatedResourceBasePaginatedItem<T> selectionListItem;

    public SearchRelatedResourceBasePaginatedWithRelatedResourceFilterWindow(String title, String filterListTitle, String filterTextTitle, String selectionListTitle, int maxResults,
            PaginatedAction filterListAction, PaginatedAction selectionListAction) {
        super(title);
        setAutoSize(true);

        // Filter section

        filterListItem = new SearchRelatedResourceBasePaginatedItem<T>("filterList", filterListTitle, FORM_ITEM_CUSTOM_WIDTH, maxResults, filterListAction);
        filterListItem.setTitleStyle("staticFormItemTitle");
        filterListItem.getListGrid().setSelectionType(SelectionStyle.SINGLE);
        filterListItem.getListGrid().addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                RelatedResourceBaseDto selectedRelatedResourceBaseDto = filterListItem.getSelectedRelatedResource();
                if (selectedRelatedResourceBaseDto != null) {
                    filterTextItem.setValue(RelatedResourceBaseUtils.getRelatedResourceName(selectedRelatedResourceBaseDto));
                } else {
                    filterTextItem.clearValue();
                }
                filterForm.markForRedraw();
            }
        });

        filterTextItem = new ViewTextItem("filterText", filterTextTitle);
        filterTextItem.setWidth(FORM_ITEM_CUSTOM_WIDTH);
        filterTextItem.setTitleColSpan(2);
        filterTextItem.setShowIfCondition(getFilterTextFormItemIfFunction());

        clearButton = new CustomButtonItem("clearButton", MetamacWebCommon.getConstants().clearSelection());
        clearButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                filterListItem.getListGrid().deselectAllRecords();
                filterTextItem.clearValue();
                filterForm.markForRedraw();
            }
        });
        clearButton.setShowIfCondition(getFilterTextFormItemIfFunction());

        filterForm = new CustomDynamicForm();
        filterForm.setCellPadding(4);
        filterForm.setTitleOrientation(TitleOrientation.TOP);
        filterForm.setMargin(15);
        filterForm.setWidth(FORM_ITEM_CUSTOM_WIDTH);
        filterForm.setNumCols(1);
        filterForm.setFields(filterListItem, filterTextItem, clearButton);

        // RelatedResource selection

        selectionListItem = new SearchRelatedResourceBasePaginatedItem<T>("list", selectionListTitle, FORM_ITEM_CUSTOM_WIDTH, maxResults, selectionListAction);
        selectionListItem.getListGrid().setSelectionType(SelectionStyle.SINGLE);
        selectionListItem.setTitleStyle("staticFormItemTitle");

        CustomButtonItem saveItem = new CustomButtonItem(FIELD_SAVE, MetamacWebCommon.getConstants().accept());

        form = new CustomDynamicForm();
        form.setTitleOrientation(TitleOrientation.TOP);
        form.setMargin(15);
        form.setWidth(FORM_ITEM_CUSTOM_WIDTH);
        form.setFields(selectionListItem, saveItem);

        addItem(filterForm);
        addItem(form);
        show();
    }

    public HasClickHandlers getSave() {
        return form.getItem(FIELD_SAVE);
    }

    public void setFilterRelatedResources(List<T> relatedResources) {
        filterListItem.setRelatedResources(relatedResources);
    }

    public void setRelatedResources(List<T> relatedResources) {
        selectionListItem.setRelatedResources(relatedResources);
    }

    public RelatedResourceBaseDto getSelectedRelatedResourceAsFilter() {
        return filterListItem.getSelectedRelatedResource();
    }

    public String getSelectedRelatedResourceUrnAsFilter() {
        return (filterListItem.getSelectedRelatedResource() != null && !StringUtils.isBlank(filterListItem.getSelectedRelatedResource().getUrn())) ? filterListItem.getSelectedRelatedResource()
                .getUrn() : null;
    }

    public T getSelectedRelatedResource() {
        return selectionListItem.getSelectedRelatedResource();
    }

    public void refreshFilterListPaginationInfo(int firstResult, int elementsInPage, int totalResults) {
        filterListItem.refreshPaginationInfo(firstResult, elementsInPage, totalResults);
    }

    public void refreshListPaginationInfo(int firstResult, int elementsInPage, int totalResults) {
        selectionListItem.refreshPaginationInfo(firstResult, elementsInPage, totalResults);
    }

    public void setFilterListSearchAction(SearchPaginatedAction action) {
        filterListItem.setSearchAction(action);
    }

    public void setSelectionListSearchAction(SearchPaginatedAction action) {
        selectionListItem.setSearchAction(action);
    }

    public String getFilterListCriteria() {
        return filterListItem.getRelatedResourceCriteria();
    }

    public String getSelectionListCriteria() {
        return selectionListItem.getRelatedResourceCriteria();
    }

    public CustomDynamicForm getFilterForm() {
        return filterForm;
    }

    public CustomDynamicForm getForm() {
        return form;
    }

    public SearchRelatedResourceBasePaginatedItem<T> getFilterListItem() {
        return filterListItem;
    }

    public SearchRelatedResourceBasePaginatedItem<T> getSelectionListItem() {
        return selectionListItem;
    }

    public CustomButtonItem getClearButton() {
        return clearButton;
    }

    private FormItemIfFunction getFilterTextFormItemIfFunction() {
        return new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return filterTextItem.getValue() != null && !StringUtils.isBlank(filterTextItem.getValue().toString());
            }
        };
    }
}

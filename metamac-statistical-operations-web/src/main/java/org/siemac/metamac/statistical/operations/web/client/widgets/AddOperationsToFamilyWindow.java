package org.siemac.metamac.statistical.operations.web.client.widgets;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.family.view.FamilyViewImpl;
import org.siemac.metamac.statistical.operations.web.client.family.view.handlers.FamilyUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.utils.ErrorUtils;
import org.siemac.metamac.web.common.client.enums.MessageTypeEnum;
import org.siemac.metamac.web.common.client.events.ShowMessageEvent;
import org.siemac.metamac.web.common.client.widgets.CustomWindow;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.actions.SearchPaginatedAction;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class AddOperationsToFamilyWindow extends CustomWindow {

    private static final int                        FORM_ITEM_CUSTOM_WIDTH = 300;

    private FamilyUiHandlers                        uiHandlers;

    private CustomDynamicForm                       form;

    private SearchOperationPaginatedDragAndDropItem operationsItem;
    private ButtonItem                              addButton;

    private List<OperationBaseDto>                  inicialSelectedOperations;

    public AddOperationsToFamilyWindow(FamilyUiHandlers familyUiHandlers) {
        super(OperationsWeb.getConstants().actionAddOperationsToFamily());
        this.uiHandlers = familyUiHandlers;

        setAutoSize(true);

        form = new CustomDynamicForm();

        operationsItem = new SearchOperationPaginatedDragAndDropItem("operationsItem", OperationsWeb.getConstants().operations(), "operationsItem", FamilyViewImpl.OPERATION_LIST_MAX_RESULTS,
                FORM_ITEM_CUSTOM_WIDTH, new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        uiHandlers.retrievePaginatedOperations(firstResult, maxResults, null);
                    }
                });
        operationsItem.setSearchAction(new SearchPaginatedAction() {

            @Override
            public void retrieveResultSet(int firstResult, int maxResults, String code) {
                uiHandlers.retrievePaginatedOperations(firstResult, maxResults, code);
            }
        });
        operationsItem.setWidth(FORM_ITEM_CUSTOM_WIDTH);

        addButton = new ButtonItem("op-save", OperationsWeb.getConstants().actionAdd());
        addButton.setColSpan(2);
        addButton.setAlign(Alignment.CENTER);
        addButton.setWidth(110);
        addButton.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                if (form.validate(false)) {
                    List<Long> selectedOperationIds = getSelectedOperationIds();
                    List<Long> initialFamilyOperationsId = getOperationIds(inicialSelectedOperations);
                    // Operations to add
                    List<Long> operationsToAdd = new ArrayList<Long>();
                    for (Long id : selectedOperationIds) {
                        if (!initialFamilyOperationsId.contains(id)) {
                            operationsToAdd.add(id);
                        }
                    }
                    // Operations to remove
                    List<Long> operationsToRemove = new ArrayList<Long>();
                    for (OperationBaseDto operationBaseDto : inicialSelectedOperations) {
                        if (!selectedOperationIds.contains(operationBaseDto.getId())) {
                            operationsToRemove.add(operationBaseDto.getId());
                        }
                    }
                    if (!operationsToAdd.isEmpty() || !operationsToRemove.isEmpty()) {
                        uiHandlers.updateFamilyOperations(operationsToAdd, operationsToRemove);
                    } else {
                        ShowMessageEvent.fire(AddOperationsToFamilyWindow.this, ErrorUtils.getMessageList(OperationsWeb.getMessages().operationAddedToFamily()), MessageTypeEnum.SUCCESS);
                    }

                    destroy();
                }
            }
        });

        form.setHeight100();
        form.setWidth100();
        form.setPadding(5);
        form.setMargin(5);
        form.setLayoutAlign(VerticalAlignment.BOTTOM);
        form.setFields(operationsItem, addButton);

        addItem(form);
        show();
    }

    public void setOperations(List<OperationBaseDto> operations, int firstResult, int totalResults) {
        operationsItem.setSourceOperations(operations);
        operationsItem.refreshSourcePaginationInfo(firstResult, operations.size(), totalResults);
    }

    public void setSelectedOperations(List<OperationBaseDto> operations) {
        this.inicialSelectedOperations = operations;
        operationsItem.setTargetOperations(operations);
    }

    public List<Long> getSelectedOperationIds() {
        return operationsItem.getSelectedOperations();
    }

    public HasClickHandlers getAdd() {
        return addButton;
    }

    public void setUiHandlers(FamilyUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }

    private List<Long> getOperationIds(List<OperationBaseDto> operationBaseDtos) {
        List<Long> operationIds = new ArrayList<Long>();
        for (OperationBaseDto operationDto : operationBaseDtos) {
            operationIds.add(operationDto.getId());
        }
        return operationIds;
    }

}

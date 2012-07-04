package org.siemac.metamac.statistical.operations.web.client.widgets;

import java.util.LinkedHashMap;
import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.family.view.FamilyViewImpl;
import org.siemac.metamac.statistical.operations.web.client.family.view.handlers.FamilyUiHandlers;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.actions.SearchPaginatedAction;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class AddOperationsToFamilyForm extends DynamicForm {

    private FamilyUiHandlers                        uiHandlers;

    private SearchOperationPaginatedDragAndDropItem operationsItem;
    private ButtonItem                              addButton;

    public AddOperationsToFamilyForm() {
        super();

        operationsItem = new SearchOperationPaginatedDragAndDropItem("operationsItem", OperationsWeb.getConstants().operations(), "operationsItem", FamilyViewImpl.OPERATION_LIST_MAX_RESULTS, 300,
                new PaginatedAction() {

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
        operationsItem.setWidth(300);

        addButton = new ButtonItem("op-save", OperationsWeb.getConstants().actionAdd());
        addButton.setColSpan(2);
        addButton.setAlign(Alignment.CENTER);
        addButton.setWidth(110);

        setHeight100();
        setWidth100();
        setPadding(5);
        setMargin(5);
        setErrorOrientation(FormErrorOrientation.RIGHT);
        setLayoutAlign(VerticalAlignment.BOTTOM);
        setFields(operationsItem, addButton);
    }

    public void setOperationsValueMap(List<OperationBaseDto> operationBaseDtos) {
        LinkedHashMap<String, String> hashMap = new LinkedHashMap<String, String>();
        for (OperationBaseDto operationBaseDto : operationBaseDtos) {
            hashMap.put(operationBaseDto.getId().toString(), operationBaseDto.getCode());
        }
        operationsItem.setValueMap(hashMap);
    }

    public void setOperations(List<OperationBaseDto> operations, int firstResult, int totalResults) {
        operationsItem.setSourceOperations(operations);
        operationsItem.refreshSourcePaginationInfo(firstResult, operations.size(), totalResults);
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

}

package org.siemac.metamac.statistical.operations.web.client.widgets;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class AddOperationsToFamilyForm extends DynamicForm {

    private SelectItem operations;
    private ButtonItem addButton;

    public AddOperationsToFamilyForm() {
        super();

        operations = new SelectItem("operations", OperationsWeb.getConstants().operations());
        operations.setWidth(300);
        operations.setMultiple(true);

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
        setFields(operations, addButton);
    }

    public void setOperationsValueMap(List<OperationBaseDto> operationBaseDtos) {
        LinkedHashMap<String, String> hashMap = new LinkedHashMap<String, String>();
        for (OperationBaseDto operationBaseDto : operationBaseDtos) {
            hashMap.put(operationBaseDto.getId().toString(), operationBaseDto.getCode());
        }
        operations.setValueMap(hashMap);
    }

    public void setOperations(List<OperationBaseDto> operationBaseDtos) {
        List<String> operationIds = new ArrayList<String>();
        for (OperationBaseDto operationBaseDto : operationBaseDtos) {
            operationIds.add(operationBaseDto.getId().toString());
        }
        operations.setValues(operationIds.toArray(new String[0]));
    }

    public List<Long> getSelectedOperationIds() {
        List<Long> operationIds = new ArrayList<Long>();
        String[] values = operations.getValues();
        for (int i = 0; i < values.length; i++) {
            Long id = Long.valueOf(values[i]);
            operationIds.add(id);
        }
        return operationIds;
    }

    public HasClickHandlers getAdd() {
        return addButton;
    }

}

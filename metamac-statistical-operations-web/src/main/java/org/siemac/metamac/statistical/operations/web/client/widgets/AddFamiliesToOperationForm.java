package org.siemac.metamac.statistical.operations.web.client.widgets;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.serviceapi.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.web.client.GopestatWeb;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class AddFamiliesToOperationForm extends DynamicForm {

    private SelectItem families;
    private ButtonItem addButton;

    public AddFamiliesToOperationForm() {
        super();

        families = new SelectItem("families", GopestatWeb.getConstants().families());
        families.setWidth(300);
        families.setMultiple(true);

        addButton = new ButtonItem("fam-save", GopestatWeb.getConstants().actionAdd());
        addButton.setAlign(Alignment.RIGHT);
        addButton.setWidth(110);

        setHeight100();
        setWidth100();
        setPadding(5);
        setMargin(5);
        setErrorOrientation(FormErrorOrientation.RIGHT);
        setLayoutAlign(VerticalAlignment.BOTTOM);
        setFields(families, addButton);
    }

    public void setFamiliesValueMap(List<FamilyBaseDto> familyBaseDtos) {
        LinkedHashMap<String, String> hashMap = new LinkedHashMap<String, String>();
        for (FamilyBaseDto familyBaseDto : familyBaseDtos) {
            hashMap.put(familyBaseDto.getId().toString(), familyBaseDto.getCode());
        }
        families.setValueMap(hashMap);
    }

    public void setFamilies(List<FamilyBaseDto> familyBaseDtos) {
        List<String> familyIds = new ArrayList<String>();
        for (FamilyBaseDto familyBaseDto : familyBaseDtos) {
            familyIds.add(familyBaseDto.getId().toString());
        }
        families.setValues(familyIds.toArray(new String[0]));
    }

    public List<Long> getSelectedFamilyIds() {
        List<Long> familyIds = new ArrayList<Long>();
        String[] values = families.getValues();
        for (int i = 0; i < values.length; i++) {
            Long id = Long.valueOf(values[i]);
            familyIds.add(id);
        }
        return familyIds;
    }

    public HasClickHandlers getAdd() {
        return addButton;
    }

}

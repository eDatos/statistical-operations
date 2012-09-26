package org.siemac.metamac.statistical.operations.web.client.widgets;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.operation.view.OperationViewImpl;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationUiHandlers;
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

public class AddFamiliesToOperationWindow extends CustomWindow {

    private static final int                     FORM_ITEM_CUSTOM_WIDTH = 300;

    private OperationUiHandlers                  uiHandlers;

    private CustomDynamicForm                    form;

    private SearchFamilyPaginatedDragAndDropItem familiesItem;
    private ButtonItem                           addButton;

    private List<FamilyBaseDto>                  initialSelectedFamilies;

    public AddFamiliesToOperationWindow(OperationUiHandlers operationUiHandlers) {
        super(getConstants().actionAddFamiliesToOperation());
        this.uiHandlers = operationUiHandlers;

        setAutoSize(true);

        form = new CustomDynamicForm();

        familiesItem = new SearchFamilyPaginatedDragAndDropItem("familiesItem", OperationsWeb.getConstants().families(), OperationViewImpl.FAMILY_LIST_MAX_RESULTS,
                FORM_ITEM_CUSTOM_WIDTH, new PaginatedAction() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults) {
                        uiHandlers.retrievePaginatedFamilies(firstResult, maxResults, null);
                    }
                });
        familiesItem.setSearchAction(new SearchPaginatedAction() {

            @Override
            public void retrieveResultSet(int firstResult, int maxResults, String code) {
                uiHandlers.retrievePaginatedFamilies(firstResult, maxResults, code);
            }
        });
        familiesItem.setWidth(FORM_ITEM_CUSTOM_WIDTH);

        addButton = new ButtonItem("fam-save", OperationsWeb.getConstants().actionAdd());
        addButton.setColSpan(2);
        addButton.setAlign(Alignment.CENTER);
        addButton.setWidth(110);
        addButton.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                if (form.validate(false)) {
                    List<Long> selectedFamilyIds = getSelectedFamilyIds();
                    List<Long> operationFamiliesId = getFamilyIds(initialSelectedFamilies);
                    // Families to add
                    List<Long> familiesToAdd = new ArrayList<Long>();
                    for (Long id : selectedFamilyIds) {
                        if (!operationFamiliesId.contains(id)) {
                            familiesToAdd.add(id);
                        }
                    }
                    // Families to remove
                    List<Long> familiesToRemove = new ArrayList<Long>();
                    for (FamilyBaseDto familyBaseDto : initialSelectedFamilies) {
                        if (!selectedFamilyIds.contains(familyBaseDto.getId())) {
                            familiesToRemove.add(familyBaseDto.getId());
                        }
                    }
                    if (!familiesToAdd.isEmpty() || !familiesToRemove.isEmpty()) {
                        uiHandlers.updateOperationFamilies(familiesToAdd, familiesToRemove);
                    } else {
                        ShowMessageEvent.fire(AddFamiliesToOperationWindow.this, ErrorUtils.getMessageList(getMessages().familiesAddedToOperation()), MessageTypeEnum.SUCCESS);
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
        form.setFields(familiesItem, addButton);

        addItem(form);
        show();
    }
    public void setFamilies(List<FamilyBaseDto> familyBaseDtos, int firstResult, int totalResults) {
        familiesItem.setSourceFamilies(familyBaseDtos);
        familiesItem.refreshSourcePaginationInfo(firstResult, familyBaseDtos.size(), totalResults);
    }

    public void setSelectedFamilies(List<FamilyBaseDto> families) {
        this.initialSelectedFamilies = families;
        familiesItem.setTargetFamilies(families);
    }

    public List<Long> getSelectedFamilyIds() {
        return familiesItem.getSelectedFamilies();
    }

    public void setUiHandlers(OperationUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }

    private List<Long> getFamilyIds(List<FamilyBaseDto> familyBaseDtos) {
        List<Long> familyIds = new ArrayList<Long>();
        for (FamilyBaseDto familyBaseDto : familyBaseDtos) {
            familyIds.add(familyBaseDto.getId());
        }
        return familyIds;
    }

}

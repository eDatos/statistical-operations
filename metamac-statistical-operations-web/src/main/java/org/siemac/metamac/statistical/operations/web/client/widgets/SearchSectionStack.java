package org.siemac.metamac.statistical.operations.web.client.widgets;

import com.smartgwt.client.types.AnimationEffect;
import com.smartgwt.client.types.Autofit;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;

public class SearchSectionStack extends SectionStack {

    private ListGrid listGrid;

    public SearchSectionStack() {
        setWidth100();
        setVisibilityMode(VisibilityMode.MULTIPLE);
        setAnimateSections(true);
        setOverflow(Overflow.VISIBLE);
        setHeight(26);
        setStyleName("versionSectionStackStyle");

        SectionStackSection section = new SectionStackSection("Buscar");
        section.setExpanded(false);

        Canvas rollUnderCanvasProperties = new Canvas();
        rollUnderCanvasProperties.setAnimateFadeTime(600);
        rollUnderCanvasProperties.setAnimateShowEffect(AnimationEffect.FADE);
        rollUnderCanvasProperties.setBackgroundColor("#ffe973");
        rollUnderCanvasProperties.setOpacity(50);

        listGrid = new ListGrid();
        listGrid.setAlternateRecordStyles(false);
        listGrid.setAutoFitMaxRecords(5);
        listGrid.setAutoFitData(Autofit.VERTICAL);
        listGrid.setRollUnderCanvasProperties(rollUnderCanvasProperties);
        listGrid.setShowRollOverCanvas(true);
        listGrid.setAnimateRollUnder(true);
        listGrid.setShowSelectionCanvas(true);
        listGrid.setAnimateSelectionUnder(true);

        ListGridField codeField = new ListGridField("ConceptSchemeDS.CODE", "conceptSchemeCode");
        codeField.setWidth("30%");
        ListGridField nameField = new ListGridField("ConceptSchemeDS.NAME", "conceptSchemeName");
        ListGridField versionField = new ListGridField("ConceptSchemeDS.VERSION_LOGIC", "conceptSchemeVersion");
        versionField.setWidth("15%");
        listGrid.setFields(codeField, nameField, versionField);

        section.setItems(listGrid);

        setSections(section);
    }

}

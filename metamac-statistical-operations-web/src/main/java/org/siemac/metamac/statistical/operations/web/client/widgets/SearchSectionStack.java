package org.siemac.metamac.statistical.operations.web.client.widgets;

import org.siemac.metamac.web.common.client.MetamacWebCommon;
import org.siemac.metamac.web.common.client.resources.GlobalResources;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomTextItem;

import com.smartgwt.client.types.AnimationEffect;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.events.HasFormItemClickHandlers;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;

public class SearchSectionStack extends SectionStack {

    private static String     SEARCH_ITEM_NAME = "search-item";

    private CustomDynamicForm form;
    private FormItemIcon      searchIcon;

    public SearchSectionStack() {
        setWidth100();
        setVisibilityMode(VisibilityMode.MULTIPLE);
        setAnimateSections(true);
        setOverflow(Overflow.VISIBLE);
        setHeight(26);
        setStyleName("versionSectionStackStyle");

        SectionStackSection section = new SectionStackSection(MetamacWebCommon.getConstants().search());
        section.setExpanded(false);

        Canvas rollUnderCanvasProperties = new Canvas();
        rollUnderCanvasProperties.setAnimateFadeTime(600);
        rollUnderCanvasProperties.setAnimateShowEffect(AnimationEffect.FADE);
        rollUnderCanvasProperties.setBackgroundColor("#ffe973");
        rollUnderCanvasProperties.setOpacity(50);

        form = new CustomDynamicForm();
        form.setMargin(5);
        form.setAutoWidth();
        CustomTextItem searchItem = new CustomTextItem(SEARCH_ITEM_NAME, "");
        searchItem.setShowTitle(false);
        searchIcon = new FormItemIcon();
        searchIcon.setSrc(GlobalResources.RESOURCE.search().getURL());
        searchItem.setIcons(searchIcon);
        form.setFields(searchItem);

        section.setItems(form);

        setSections(section);
    }

    public HasFormItemClickHandlers getSearchIcon() {
        return searchIcon;
    }

    public String getSearchCriteria() {
        return form.getValueAsString(SEARCH_ITEM_NAME);
    }

    public void reset() {
        form.clearValues();
        collapseSection(0);
    }

}

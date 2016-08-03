package org.siemac.metamac.statistical.operations.web.client.view.handlers;

import com.gwtplatform.mvp.client.UiHandlers;

public interface MainPageUiHandlers extends UiHandlers {

    void openHelpUrl();
    void closeSession();

    void goToFamilies();
    void goToOperations();

    void onNavigationPaneSectionHeaderClicked(String name);
    void onNavigationPaneSectionClicked(String name);
}

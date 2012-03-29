package org.siemac.metamac.gopestat.web.client.gin;

import org.siemac.metamac.gopestat.web.client.GopestatWebConstants;
import org.siemac.metamac.gopestat.web.client.GopestatWebMessages;
import org.siemac.metamac.gopestat.web.client.family.presenter.FamilyListPresenter;
import org.siemac.metamac.gopestat.web.client.family.presenter.FamilyPresenter;
import org.siemac.metamac.gopestat.web.client.instance.presenter.InstancePresenter;
import org.siemac.metamac.gopestat.web.client.operation.presenter.OperationListPresenter;
import org.siemac.metamac.gopestat.web.client.operation.presenter.OperationPresenter;
import org.siemac.metamac.gopestat.web.client.presenter.MainPagePresenter;

import com.google.gwt.event.shared.EventBus;
import com.google.gwt.inject.client.AsyncProvider;
import com.google.gwt.inject.client.GinModules;
import com.google.gwt.inject.client.Ginjector;
import com.google.inject.Provider;
import com.gwtplatform.dispatch.client.gin.DispatchAsyncModule;
import com.gwtplatform.mvp.client.proxy.PlaceManager;

@GinModules({DispatchAsyncModule.class, ClientModule.class})
public interface GopestatWebGinjector extends Ginjector {

    PlaceManager getPlaceManager();
    EventBus getEventBus();

    Provider<MainPagePresenter> getMainPagePresenter();

    AsyncProvider<FamilyListPresenter> getFamilyListPresenter();
    AsyncProvider<FamilyPresenter> getFamilyPresenter();
    AsyncProvider<OperationListPresenter> getOperationListPresenter();
    AsyncProvider<OperationPresenter> getOperationPresenter();
    AsyncProvider<InstancePresenter> getInstancePresenter();

    // Interfaces
    public GopestatWebConstants getGopestatWebConstants();
    public GopestatWebMessages getGopestatWebMessages();

}

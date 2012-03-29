package org.siemac.metamac.statistical.operations.web.client.gin;

import org.siemac.metamac.statistical.operations.web.client.OperationsWebConstants;
import org.siemac.metamac.statistical.operations.web.client.OperationsWebMessages;
import org.siemac.metamac.statistical.operations.web.client.family.presenter.FamilyListPresenter;
import org.siemac.metamac.statistical.operations.web.client.family.presenter.FamilyPresenter;
import org.siemac.metamac.statistical.operations.web.client.instance.presenter.InstancePresenter;
import org.siemac.metamac.statistical.operations.web.client.operation.presenter.OperationListPresenter;
import org.siemac.metamac.statistical.operations.web.client.operation.presenter.OperationPresenter;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;

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
    public OperationsWebConstants getOperationsWebConstants();
    public OperationsWebMessages getOperationsWebMessages();

}

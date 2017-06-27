#!/bin/sh

HOME_PATH=metamac-statistical-operations
TRANSFER_PATH=$HOME_PATH/tmp
DEPLOY_TARGET_PATH=/servers/metamac/tomcats/metamac01/webapps
ENVIRONMENT_RELATIVE_PATH_FILE=WEB-INF/classes/metamac/environment.xml
LOGBACK_RELATIVE_PATH_FILE=WEB-INF/classes/logback.xml
RESTART=1

if [ "$1" == "--no-restart" ]; then
    RESTART=0
fi


scp -r etc/deploy deploy@estadisticas.arte-consultores.com:$TRANSFER_PATH
scp metamac-statistical-operations-web/target/statistical-operations-internal-*.war deploy@estadisticas.arte-consultores.com:$TRANSFER_PATH/statistical-operations-internal.war
scp metamac-statistical-operations-external-web/target/statistical-operations-*.war deploy@estadisticas.arte-consultores.com:$TRANSFER_PATH/statistical-operations.war
ssh deploy@estadisticas.arte-consultores.com <<EOF

    chmod a+x $TRANSFER_PATH/deploy/*.sh;
    . $TRANSFER_PATH/deploy/utilities.sh

    if [ $RESTART -eq 1 ]; then
        sudo service metamac01 stop
        checkPROC "metamac"
    fi

    ###
    # STATISTICAL-OPERATIONS-INTERNAL
    ###

    # Update Process
    sudo rm -rf $DEPLOY_TARGET_PATH/statistical-operations-internal
    sudo mv $TRANSFER_PATH/statistical-operations-internal.war $DEPLOY_TARGET_PATH/statistical-operations-internal.war
    sudo unzip $DEPLOY_TARGET_PATH/statistical-operations-internal.war -d $DEPLOY_TARGET_PATH/statistical-operations-internal
    sudo rm -rf $DEPLOY_TARGET_PATH/statistical-operations-internal.war

    # Restore Configuration
    sudo cp $HOME_PATH/environment_internal.xml $DEPLOY_TARGET_PATH/statistical-operations-internal/$ENVIRONMENT_RELATIVE_PATH_FILE
    sudo cp $HOME_PATH/logback_internal.xml $DEPLOY_TARGET_PATH/statistical-operations-internal/$LOGBACK_RELATIVE_PATH_FILE


    ###
    # STATISTICAL-OPERATIONS-EXTERNA
    ###

    # Update Process
    sudo rm -rf $DEPLOY_TARGET_PATH/statistical-operations
    sudo mv $TRANSFER_PATH/statistical-operations.war $DEPLOY_TARGET_PATH/statistical-operations.war
    sudo unzip $DEPLOY_TARGET_PATH/statistical-operations.war -d $DEPLOY_TARGET_PATH/statistical-operations
    sudo rm -rf $DEPLOY_TARGET_PATH/statistical-operations.war

    # Restore Configuration
    sudo cp $HOME_PATH/environment.xml $DEPLOY_TARGET_PATH/statistical-operations/$ENVIRONMENT_RELATIVE_PATH_FILE
    sudo cp $HOME_PATH/logback.xml $DEPLOY_TARGET_PATH/statistical-operations/$LOGBACK_RELATIVE_PATH_FILE

    if [ $RESTART -eq 1 ]; then
        sudo chown -R metamac.metamac /servers/metamac
        sudo service metamac01 start
    fi

    #checkURL "http://estadisticas.arte-consultores.com/statistical-operations-internal" "metamac01"
    #checkURL "http://estadisticas.arte-consultores.com/statistical-operations" "metamac01"
    echo "Finished deploy"

EOF
